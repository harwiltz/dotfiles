#!/usr/bin/env bash

if [[ $(xset q | grep "DPMS is Enabled") ]]; then
  xset s off -dpms
else
  xset s on +dpms
fi
