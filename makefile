.PHONY: default
default:
	echo "make tutor to make the demonic tutor"
	echo "make player-* to make the player-*.sml"

.PHONY: tutor
tutor:
	mlton -output tutor eternal/demonic-tutor.cm

# TODO: I think this strategy of building is kind of confusing,
# since if player.sml doesn't compile, it reports errors there
# and tempts you to make edits to it, which are then lost when
# you do make again. We can probably improve it... -tom7
#
# XXX: Also this doesn't seem to rebuild if anything but player-*.sml
# changes?
.PHONY: player-*
player-%:
	echo > player.sml
	echo '(* DO NOT EDIT! Instead edit source' $@.sml '! *)' >> player.sml
	echo >> player.sml
	cat $@.sml >> player.sml
	echo >> player.sml
	echo '(* DO NOT EDIT! Instead edit source' $@.sml '! *)' >> player.sml
	echo >> player.sml
	mlton -output $@.exe player.cm
	rm player.sml
