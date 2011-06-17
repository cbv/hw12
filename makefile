.PHONY: default
default:
	echo "make tutor to make the demonic tutor"
	echo "make player-* to make the player-*.sml"

.PHONY: tutor
tutor:
	mlton -output tutor eternal/demonic-tutor.cm

.PHONY: player-*
player-%: 
	cp $@.sml player.sml
	mlton -output $@.exe player.cm
	rm player.sml