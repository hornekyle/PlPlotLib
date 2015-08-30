program main_prg
	use kinds_mod
	use plplotlib_mod
	use examples_mod
	implicit none
	
!~ 	call setup(whiteOnBlack=.false.)

	call setup(device='svgqt',fileName='examples/example-%n.svg',figSize=[320,240],transparent=.true.)
!~ 	call setup(device='svgqt',fileName='examples/logo-%n.svg',figSize=[500,500])
	
	call doExamples()
!~ 	call makeLogo
	call show()

end program main_prg
