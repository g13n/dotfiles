#!/usr/sbin/bash
# This is the accompanying example script file for rcirc-alert.el
#
# This file accepts four input parameters (text strings).
# - type of notification
# - Title of notification
# - Text of notification
# - Buffer (IRC channel)
#
# It uses the naughty library of awesome window manager to display messages

case $1 in
    "message")
        T1='naughty.notify({title = "'
        T2='", text = "'
        T3='", timeout = 4, position="top_right", icon="/home/csantos/Pictures/icons/chat-icon.png", fg="#000000", bg="#17F901"})'
        ;;
    "keyword")
        T1='naughty.notify({title = "'
        T2='", text = "'
        T3='", timeout = 4, position="top_right", icon="/home/csantos/Pictures/icons/keywords.png", fg="#000000", bg="#E34CD8"})'
        ;;
    "private")
        T1='naughty.notify({title = "'
        T2='", text = "'
        T3='", timeout = 4, position="top_right", icon="/home/csantos/Pictures/icons/private.png", fg="#000000", bg="#22BFCF"})'
        ;;
    "nick")
        T1='naughty.notify({title = "'
        T2='", text = "'
        T3='", timeout = 4, position="top_right", icon="/home/csantos/Pictures/icons/chat-icon.png", fg="#000000", bg="#E34CD8"})'
        ;;
    "always")
        T1='naughty.notify({title = "'
        T2='", text = "'
        T3='", timeout = 15, position="top_right", icon="/home/csantos/Pictures/icons/twitter.jpg", fg="#000000", bg="#00FFFF"})'
        ;;
    *)
        ;;
esac

printf -v texto '%s %-15s %-15s %-15s %-15s %-15s\n' "$T1" $2 "    @ " $4 "$T2" $3 "$T3"
echo $texto | awesome-client
