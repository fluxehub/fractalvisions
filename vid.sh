#!/usr/bin/env bash
rm *.mp4 out/*
stack run
ffmpeg -framerate 60 -i out/frame%04d.png output.mp4
ffmpeg -i output.mp4 -i song.wav -c:v libx264 -c:a libvorbis -shortest test.mp4