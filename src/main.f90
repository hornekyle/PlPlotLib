program main_prg
	use kinds_mod
	use plplotlib_mod
	use test_mod
	implicit none
	
	call setup()
!~ 	call setup(device='svgqt',fileName='examples/example-%n.svg')
!~ 	call setup(device='svgqt',fileName='examples/logo-%n.svg',figSize=[500,500])
	
	call runTests
!~ 	call makeLogo
	
	call show

end program main_prg
