.PHONY: default
default:
	echo "make player-* to make the player-*.sml"

.PHONY: player-*
player-%: 
	cp $@.sml player.sml
	mlton -output $@.exe player.cm
	rm player.sml