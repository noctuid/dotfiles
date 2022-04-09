#! /usr/bin/env bash
# https://askubuntu.com/questions/545284/ufw-ctrl-c-and-iptables-chain-already-exists
set -e
set -o pipefail

iptables --flush
rules=($(iptables --list | grep Chain | grep -Eo "ufw-[a-z-]+" | xargs echo))
for i in "${rules[@]}"
do
  iptables --delete-chain $i
done

ip6tables --flush
rules6=($(ip6tables --list | grep Chain | grep -Eo "ufw6-[a-z-]+" | xargs echo))
for i in "${rules6[@]}"
do
  ip6tables --delete-chain $i
done
