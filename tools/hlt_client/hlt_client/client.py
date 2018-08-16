#!/usr/bin/env python3

import re
import os
import sys
import json
import argparse

from . import output
from . import upload_bot
from . import download_game
from . import compare_bots
from . import gym

"""client.py: Client for interacting with the Halite II servers."""

__author__ = "Two Sigma"
__copyright__ = "Copyright 2017, Two Sigma"
__credits__ = ["David M. Li", "Jaques Clapauch"]
__date__ = "August 1, 2017"
__email__ = "halite@halite.io"
__license__ = "MIT"
__status__ = "Production"
__version__ = "1.5"

URI_HALITE_API = 'http://35.190.92.101/v1/api'
URI_API_CREATE_BOT = URI_HALITE_API + "/user/{}/bot"
URI_API_EXISTING_BOT = URI_HALITE_API + "/user/{}/bot/{}"
URI_HALITE_WEB_PAGE = 'http://halite.io'
URI_WEB_API_KEY = "{}/user/settings".format(URI_HALITE_WEB_PAGE)

CONFIG_DIR = 'HALITE_CONFIG_DIR'

SUCCESS = 200
FIRST_BOT_ID = 0
BOT_FILE_KEY = 'botFile'
API_KEY_HEADER = 'X-API-KEY'

AUTH_MODE = 'auth'
GYM_MODE = 'gym'
PLAY_MODE = 'play'
REPLAY_MODE = 'replay'
BOT_MODE = 'bot'
BOT_UPLOAD_MODE = 'upload'
BOT_DOWNLOAD_MODE = 'download'
MODES = str({AUTH_MODE, GYM_MODE, PLAY_MODE, REPLAY_MODE, BOT_MODE})
REPLAY_MODE_DATE = 'date'
REPLAY_MODE_USER = 'user'


class Config:
    _key_example = "<username>:<key>"
    _key_delimiter = ':'
    _user_position = 0

    _user_key = 'user'
    _api_key_key = 'api_key'

    def __init__(self, auth=None):
        self._config_folder = self._get_config_folder_path()
        self._auth_file = self._get_auth_file_path()
        if not os.path.exists(self._config_folder):
            os.makedirs(self._config_folder)
        if auth:
            auth = self._parse_api_key(auth)
            self._write_auth(auth)
        else:
            auth = self._get_auth_json()
        self.api_key = auth[self._api_key_key]
        self.user_id = auth[self._user_key]

    @staticmethod
    def _get_config_folder_path():
        """
        Returns the folder for config.

        :return:  %LOCALAPPDATA%/Halite if windows ~/.config/hlt otherwise
                  The HALITE_CONFIG_DIR environment variable overrides
                  these defaults. If set, returns its value.
        """
        if os.getenv(CONFIG_DIR):
            return os.getenv(CONFIG_DIR)

        if sys.platform == 'win32':
            return "{}/Halite".format(os.getenv('LOCALAPPDATA'))

        return "{}/.config/hlt".format(os.path.expanduser("~"))

    @staticmethod
    def _get_auth_file_path():
        """
        :return: Auth file location where configs will be written to in JSON format
        """
        return "{}/auth".format(Config._get_config_folder_path())

    @staticmethod
    def auth_exists():
        """
        Whether tha auth file has been created already
        :return: True if exists, False otherwise
        """
        return os.path.isfile(Config._get_auth_file_path())

    def _write_auth(self, data):
        """
        Writes to auth file the desired data. Expected that the input be in JSON format.
        :param data: Data to be written to the auth file
        :return: Nothing
        """
        config_file = open(self._auth_file, 'w')
        config_file.writelines(json.dumps(data))

    def _get_auth_json(self):
        """
        Returns the auth JSON object as acquired from the auth file. If none, throws exception asking the user
        to first authenticate.
        :return: The JSON object with the auth information
        """
        if not self.auth_exists():
            raise ValueError("CLI not authenticated. Please run `client.py auth` first.")
        with open(self._auth_file) as file:
            config_contents = file.read()
        try:
            return json.loads(config_contents)
        except (TypeError, ValueError):
            raise ValueError("Secret formatting has been mangled. Try re-authenticating (`client.py --auth`).")

    @staticmethod
    def _parse_api_key(api_key):
        """
        Determines if the API key supplied is valid via regex. Returns the parsed contents in a dict (user and key)
        :param api_key: The string containing the API key
        :return: A dict containing the parse contents of the api key (user and key)
        """
        config_result = {}
        key_regex = re.compile("\d+:[0-9a-fA-F]{32}")
        if not api_key or not re.match(key_regex, api_key):
            raise ValueError("Malformed API Key. Expected {}".format(Config._key_example))
        config_result[Config._api_key_key] = api_key
        config_result[Config._user_key] = api_key.split(Config._key_delimiter)[Config._user_position]
        return config_result

    def __str__(self):
        return "* id:\t\t{}{}* api_key:\t{}".format(self.user_id, os.linesep, self.api_key)

    def to_json(self):
        return { "id": self.user_id, "api_key": self.api_key }

    def __repr__(self):
        return self.__str__()


def _parse_arguments():
    """
    Simple argparser
    :return: parsed arguments if any. Prints help otherwise
    """
    parser = argparse.ArgumentParser(description="Halite 2.0 CLI")
    parser.add_argument('--json', action='store_true', help="Disable interactivity and output as JSON.")
    # .Modes
    subparser = parser.add_subparsers(dest='mode', metavar=MODES)
    # .Modes.Auth
    auth_parser = subparser.add_parser(AUTH_MODE, help='Authorize client to make requests on your behalf')
    auth_parser.add_argument('-m', '--metadata', action='store_true', help="Print auth metadata")
    auth_parser.add_argument('-k', '--key', help="Specify API key (meant for use with --json)")
    # .Modes.Bot
    bot_parser = subparser.add_parser('bot', help='Actions associated with a bot')
    bot_subparser = bot_parser.add_subparsers(dest='bot_mode', required=True)
    bot_upload_parser = bot_subparser.add_parser(BOT_UPLOAD_MODE, help='Actions associated with a bot')
    bot_download_parser = bot_subparser.add_parser(BOT_DOWNLOAD_MODE, help='Actions associated with a bot')
    bot_parser.add_argument('-b', '--bot-path', dest='bot_path', action='store', type=str, required=True,
                            help="The path wherein your bot zip lives.")
    bot_upload_parser.add_argument('--dry-run', dest='dry_run', action='store_true',
                                   help="Verify the bot upload but do not actually upload.")
    bot_upload_parser.add_argument('-i', '--include',
                                   dest='include_extensions',
                                   action='append',
                                   type=str,
                                   required=False,
                                   help="Include files with these extensions in the upload. May pass 0 or more times (if not given, defaults to Python/Java/C++)")
    # .Modes.Play
    compare_bots.parse_arguments(subparser)
    # .Modes.Gym
    gym.parse_arguments(subparser)
    # .Modes.Replay
    replay_parser = subparser.add_parser('replay', help='Actions associated with replay files')
    # .Modes.Replay.Modes
    replay_subparser = replay_parser.add_subparsers(dest='replay_mode', metavar='{date, user}')
    # .Modes.Replay.Modes.User
    replay_user_parser = replay_subparser.add_parser(REPLAY_MODE_USER, help='Retrieve replays based on a specified user')
    replay_user_parser.add_argument('-i', '--id', action='store', dest='user_id',
                                    help="Fetch recent replay files apposite a user. "
                                         "Enter a user id to fetch that specific"
                                         "user's files; leave blank to fetch yours")
    replay_user_parser.add_argument('-l', '--limit', action='store', dest='limit', type=int, default=250,
                                    help='Number of replays to fetch')
    replay_user_parser.add_argument('-d', '--destination', dest='destination', action='store', type=str, required=True,
                                    help="In which folder to store all resulting replay files.")
    # .Modes.Replay.Modes.Date
    replay_regex_parser = replay_subparser.add_parser(REPLAY_MODE_DATE, help='Retrieve replays based on regex')
    replay_regex_parser.add_argument('-t', '--date', action='store', type=str, dest='date', required=True,
                                     help="Fetch replay files matching the specified date. To fetch a day's files user"
                                          "the YYYYMMDD format.")
    replay_regex_parser.add_argument('-a', '--all', action='store_true', default=False,
                                     help="Whether to retrieve all files. Omit for only Gold and higher.")
    replay_regex_parser.add_argument('-d', '--destination', dest='destination', action='store', type=str, required=True,
                                     help="In which folder to store all resulting replay files.")
    if len(sys.argv) < 2:
        parser.print_help()
    return parser.parse_args()


def authorize():
    """
    Create the config for the user. This will ask the user to visit a webpage and paste the api key encountered.
    :return: Nothing
    """

    if output.mode() == output.JSON:
        output.error("Not logged in.")
        sys.exit(1)

    api_key = input("Please go to {} to obtain an api_key, and paste here: ".format(URI_WEB_API_KEY))
    Config(api_key)
    print("Successfully set up user account")


def main():
    """
    Main function gets the args input and determines which method to call to handle. Handles exceptions from
    malformed input.
    :return: Nothing
    """
    try:
        args = _parse_arguments()

        if args.json:
            output.set_mode('json')

        if args.mode == AUTH_MODE:
            if args.key:
                Config(args.key)
                output.output("Saved API key.")
                return

            if not (args.metadata and Config.auth_exists()):
                authorize()
            if args.metadata:
                output.output(Config())
        elif args.mode == BOT_MODE:
            if args.bot_mode == BOT_UPLOAD_MODE:
                upload_bot.upload(args.bot_path, args.dry_run, args.include_extensions)
            else:
                upload_bot.download(args.bot_path)
        elif args.mode == REPLAY_MODE:
            download_game.download(args.replay_mode, args.destination,
                                   getattr(args, 'date', None), getattr(args, 'all', None),
                                   Config().user_id if Config.auth_exists() else None, getattr(args, 'user_id', None),
                                   getattr(args, 'limit', None))
        elif args.mode == PLAY_MODE:
            compare_bots.play_games(args.halite_binary,
                                    args.game_output_dir,
                                    args.map_width, args.map_height,
                                    args.run_commands, args.iterations, [])
        elif args.mode == GYM_MODE:
            gym.main(args)
    except (IndexError, TypeError, ValueError, IOError) as err:
        output.error(str(err))
        exit(-1)


if __name__ == "__main__":
    main()
