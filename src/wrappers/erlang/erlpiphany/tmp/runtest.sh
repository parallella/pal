#!/bin/bash
/opt/adapteva/esdk/setup.sh
sudo erl -s ehal init -s ehal_test open_test -s init stop
