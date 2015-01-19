#!/bin/bash
$1 $2
arm-linux-gnueabihf-gcc -mfpu=neon-vfpv4  -mcpu=cortex-a15 -static main.S output.s
