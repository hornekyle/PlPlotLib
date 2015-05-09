program main_prg
	use kinds_mod
	use plplot_mod
	implicit none
	
	call setup(colormap='BlueRed')
	call testPlot
	call testScatter
	call testContour
	call testLegend
	call show
	
contains

	subroutine testPlot
		integer,parameter::N = 20
		real(wp),dimension(N)::x,y
		integer::k
		
		x = [( real(k-1,wp)/real(N-1,wp) , k=1,N )]
		y = x**2-1.0_wp
		
		call figure()
		call subplot(1,1,1)
		call xylim(mixval(x),mixval(y))
		
		call plot(x,y,lineColor='red',lineWidth=2.0_wp, &
			& markStyle='.',markColor='cyan',markSize=2.0_wp)
		
		call plot(x,-1.0_wp-y,lineColor='blue',lineStyle=':',lineWidth=2.0_wp, &
			& markStyle='+',markColor='green',markSize=1.0_wp)
		
		call ticks()
		call labels('x','y','f(x)=x#u2#d-1; g(x)=-x#u2#d')
	end subroutine testPlot

	subroutine testScatter
		integer,parameter::N = 100
		real(wp),dimension(N)::x,y,z
		
		call random_number(x)
		call random_number(y)
		z = sqrt(x**2+y**2)
		
		call figure()
		
		call subplot(2,2,1)
		call xylim([0.0_wp,1.0_wp],[0.0_wp,1.0_wp])
		call scatter(x,y)
		call ticks()
		call labels('x','y','')
		
		call subplot(2,2,2)
		call xylim([0.0_wp,1.0_wp],[0.0_wp,1.0_wp])
		call scatter(x,y,c=z)
		call ticks()
		call labels('x','y','')
		
		call subplot(2,2,3)
		call xylim([0.0_wp,1.0_wp],[0.0_wp,1.0_wp])
		call scatter(x,y,s=(4.0_wp*z+1.0_wp),markColor='blue')
		call ticks()
		call labels('x','y','')
		
		call subplot(2,2,4)
		call xylim([0.0_wp,1.0_wp],[0.0_wp,1.0_wp])
		call scatter(x,y,c=z,s=(4.0_wp*z+1.0_wp))
		call ticks()
		call labels('x','y','')
	end subroutine testScatter

	subroutine testContour
		integer,parameter::N = 100
		real(wp),dimension(N)::x,y
		real(wp),dimension(N,N)::z
		integer::i,j
		
		x = 20.0_wp*[( real(i-1,wp)/real(N-1,wp) , i=1,N )]-10.0_wp
		y = 20.0_wp*[( real(j-1,wp)/real(N-1,wp) , j=1,N )]-10.0_wp
		forall(i=1:N,j=1:N)
			z(i,j) = sin( sqrt(x(i)**2+y(j)**2) )/sqrt(x(i)**2+y(j)**2)
		end forall
		
		call figure()
		
		call subplot(1,1,1,aspect=1.0_wp)
		call xylim(mixval(x),mixval(y))
		call contourf(x,y,z,50)
		call contour(x,y,z,10)
		call ticks()
		call labels('x','y','')
	end subroutine testContour

	subroutine testLegend
		integer,parameter::N = 20
		real(wp),dimension(N)::x,y
		character(32),dimension(2,6)::series
		integer::k
		
		x = [( real(k-1,wp)/real(N-1,wp) , k=1,N )]
		y = x**2-1.0_wp
		
		call figure()
		call subplot(1,1,1)
		call xylim(mixval(x),mixval(y))
		
		call plot(x,y,lineColor='red',lineWidth=2.0_wp, &
			& markStyle='.',markColor='cyan',markSize=2.0_wp)
		
		call plot(x,-1.0_wp-y,lineColor='blue',lineStyle=':',lineWidth=2.0_wp, &
			& markStyle='+',markColor='green',markSize=1.0_wp)
		
		! [name,textColor,lineStyle,lineColor,markStyle,markColor]
		series(1,:) = [character(32)::'f(x)=x#u2#d-1','k','-','r','.','c']
		series(2,:) = [character(32)::'g(x)=-x#u2#d','k',':','b','+','g']
		
		call legend('center left',series)
		call ticks()
		call labels('x','y','')
	end subroutine testLegend

end program main_prg
