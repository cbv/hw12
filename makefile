.PHONY: default
default:
	echo "make tutor to make the demonic tutor"
	echo "make player-*.exe to make the player-*.sml"

.PHONY: tutor
tutor:
	mlton -const "Exn.keepHistory true" -output tutor eternal/demonic-tutor.cm

# TODO: I think this strategy of building is kind of confusing,
# since if player.sml doesn't compile, it reports errors there
# and tempts you to make edits to it, which are then lost when
# you do make again. We can probably improve it... -tom7
#
# XXX: Also this doesn't seem to rebuild if anything but player-*.sml
# changes?
player-%.tar.gz: player-%.exe
	@mkdir -p $<-build
	@cp $< $<-build/run
	@touch $<-build/install
	@chmod +x $<-build/install
	@echo '*** THIS IS OKAY FOR THE ARENA, BUT WE MUST COPY SOURCE IN FOR THE FINAL SUBMISSION! ***'
	tar czvf $@ -C $<-build run install
	@echo '*** THIS IS OKAY FOR THE ARENA, BUT WE MUST COPY SOURCE IN FOR THE FINAL SUBMISSION! ***'
	@rm -rf $<-build

player-%.exe: .DUMMY
	@rm -f player.sml
	@echo '(* DO NOT EDIT! Instead edit source' $(@:exe=sml) '! *)' >> player.sml
	@echo >> player.sml
	@cat $(@:exe=sml) >> player.sml
	@echo >> player.sml
	@echo '(* DO NOT EDIT! Instead edit source' $(@:exe=sml) '! *)' >> player.sml
	@echo >> player.sml
	@svn info | ./embedversion.pl > version.sml
	@chmod a-w player.sml # try to prevent user from editing
	mlton -cc-opt -static -link-opt -static -verbose 1 -const "Exn.keepHistory true" -output $@ player.cm
	@chmod u+w player.sml
	@rm -f player.sml
	@rm version.sml

.PHONY: .DUMMY
.DUMMY:
