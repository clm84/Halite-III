#!/bin/sh

stack install

./halite --replay-directory replays/ -vvv --width 32 --height 32 "MyBot" "MyBot"
