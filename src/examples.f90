module examples_mod
	!! Examples of module use
	use kinds_mod
	use plplotlib_mod
	implicit none
	private
	
	public::makeLogo
	public::doExamples
	
contains

	subroutine makeLogo
		real(wp),dimension(:),allocatable::x,y1,y2,y3
		
		x  = linspace(0.0_wp,1.0_wp,100)
		y1 = x**2-1.0_wp
		y2 = 2.0_wp*x-1.0_wp
		y3 = x
		y3 = cos(2.0_wp*PI*x)
		
		call figure()
		call subplot(1,1,1)
		call xylim(mixval(x),mixval([y1,y2,y3])*1.1_wp)
		
		call plot(x,y1,lineColor='b',lineWidth=10.0_wp)
		call plot(x,y2,lineColor='r',lineWidth=10.0_wp)
		call plot(x,y3,lineColor='c',lineWidth=10.0_wp)
		
		call ticks(lineWidth=5.0_wp)
		call labels('','','')
	end subroutine makeLogo

	subroutine doExamples
		call doPlot
		call doScatter
		call doContour
		call doLegend
		call doQuiver
		call doBar
		call doFillBetween
		call doHist
		call doSurface
	end subroutine doExamples

	subroutine doPlot
		integer,parameter::N = 20
		real(wp),dimension(N)::x,y
		
		x = linspace(0.0_wp,1.0_wp,N)
		y = x**2-1.0_wp
		
		call figure()
		call subplot(1,1,1)
		call xylim(mixval(x),mixval(y))
		
		call plot(x,y,lineColor='red',lineWidth=2.0_wp, &
			& markStyle='.',markColor='cyan',markSize=2.0_wp)
		
		call plot(x,-1.0_wp-y,lineColor='blue',lineStyle=':',lineWidth=2.0_wp, &
			& markStyle='+',markColor='green',markSize=1.0_wp)
		
!~ 		call ticks()
		call xticks(primary=.true.,secondary=.false.)
		call yticks(primary=.true.,secondary=.false.)
!~ 		call labels('x','y','f(x)=x#u2#d-1; g(x)=-x#u2#d')
		call xlabel('x')
		call ylabel('y')
		call title('f(x)=x#u2#d-1; g(x)=-x#u2#d')
	end subroutine doPlot

	subroutine doScatter
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
	end subroutine doScatter

	subroutine doContour
		integer,parameter::N = 100
		real(wp),dimension(N)::x,y
		real(wp),dimension(N,N)::z
		integer::i,j
		
		x = linspace(-10.0_wp,10.0_wp,N)
		y = linspace(-10.0_wp,10.0_wp,N)
		forall(i=1:N,j=1:N)
			z(i,j) = sin( sqrt(x(i)**2+y(j)**2) )/sqrt(x(i)**2+y(j)**2)
		end forall
		
		call figure()
		
		call subplot(1,1,1,aspect=1.0_wp)
		call xylim(mixval(x),mixval(y))
		call contourf(x,y,z,50)
		call contour(x,y,z,10)
		call colorbar(z,5)
		call ticks()
		call labels('x','y','')
	end subroutine doContour

	subroutine doLegend
		integer,parameter::N = 20
		real(wp),dimension(N)::x,y
		character(32),dimension(3,7)::series
		
		x = linspace(0.0_wp,1.0_wp,N)
		y = x**2-1.0_wp
		
		call figure()
		call subplot(1,1,1)
		call xylim(mixval(x),mixval(y))
		
		call plot(x,y,lineColor='red',lineWidth=2.0_wp, &
			& markStyle='.',markColor='cyan',markSize=2.0_wp)
		
		call plot(x,-1.0_wp-y,lineColor='blue',lineStyle=':',lineWidth=2.0_wp, &
			& markStyle='+',markColor='green',markSize=1.0_wp)
		
		! [name,textColor,lineStyle,lineColor,markStyle,markColor]
		series(1,:) = [character(32)::'f(x)=x#u2#d-1','','-','r','.','c','']
		series(2,:) = [character(32)::'g(x)=-x#u2#d','',':','b','+','g','']
		series(3,:) = [character(32)::'Box','','','','','','r']
		
		call legend('center left',series)
		call ticks()
		call labels('x','y','')
	end subroutine doLegend

	subroutine doQuiver
		integer,parameter::N = 20
		real(wp),dimension(N)::x,y
		real(wp),dimension(N,N)::u,v,m
		integer::i,j
		
		x = linspace(-10.0_wp,10.0_wp,N)
		y = linspace(-10.0_wp,10.0_wp,N)
		forall(i=1:N,j=1:N)
			u(i,j) = -y(j)
			v(i,j) =  x(i)
			m(i,j) = sqrt(u(i,j)**2+v(i,j)**2)
		end forall
		
		call figure()
		
		call subplot(1,1,1,aspect=1.0_wp)
		call xylim(mixval(x),mixval(y))
		call quiver(x,y,u,v,c=m,s=m,scaling=2.0_wp,lineWidth=2.0_wp)
		call colorbar(m,10)
		call ticks()
		call labels('x','y','')
	end subroutine doQuiver

	subroutine doBar
		integer,parameter::N = 21
		real(wp),dimension(N)::x,y
		
		x = linspace(-PI,PI,N)
		y = exp(-x**2)
		
		call figure()
		
		call subplot(1,2,1)
		call xylim(mixval(x)+[-0.1_wp,0.1_wp],mixval(y)+[ 0.0_wp,0.1_wp])
		call bar(x,y,c=y,relWidth=1.0_wp)
		call ticks()
		call labels('x','y','')
		
		call subplot(1,2,2)
		call xylim(mixval(y)+[ 0.0_wp,0.1_wp],mixval(x)+[-0.1_wp,0.1_wp])
		call barh(x,y,fillColor='r',relWidth=1.0_wp)
		call ticks()
		call labels('x','y','')
	end subroutine doBar

	subroutine doFillBetween
		integer,parameter::N = 51
		real(wp),dimension(N)::x,y1,y2
		
		x = linspace(-3.0_wp,3.0_wp,N)
		y1 = x**2-1.0_wp
		y2 = x**3-1.0_wp
		
		call figure()
		call subplot(1,1,1)
		call xylim(mixval(x),mixval([y1,y2]))
		call fillBetween(x,y1,y2,fillColor='c',fillPattern='\',lineWidth=2.0_wp)
		call plot(x,y1,lineColor='k',lineWidth=3.0_wp)
		call plot(x,y2,lineColor='k',lineWidth=3.0_wp)
		call ticks(color='b',lineWidth=3.0_wp)
		call labels('x','y','f(x)=x#u2#d-1',color='r')
	end subroutine doFillBetween

	subroutine doHist
		integer,parameter::N = 1000
		real(wp),dimension(N,12)::r
		real(wp),dimension(N)::x
		real(wp),dimension(:,:),allocatable::h
		
		call random_number(r)
		x = sum(r,2)-6.0_wp
		call figure()
		
		call subplot(1,2,1)
		call xylim(mixval(x),[0.0_wp,1.05_wp])
		call hist(x,100)
		call ticks()
		
		h = binData(x,100,normalize=2)
		call subplot(1,2,2)
		call xylim(mixval(h(:,1)),[0.0_wp,1.05_wp*maxval(h(:,2))])
		call bar(h(:,1),h(:,2),c=h(:,2),relWidth=1.0_wp)
		call ticks()
	end subroutine doHist

	subroutine doSurface
		use plplot
		
		integer,parameter::N = 150
		real(wp),dimension(N)::x,y
		real(wp),dimension(N,N)::z
		integer::i,j
		
		x = linspace(-10.0_wp,10.0_wp,N)
		y = linspace(-10.0_wp,10.0_wp,N)
		forall(i=1:N,j=1:N)
			z(i,j) = sin( sqrt(x(i)**2+y(j)**2) )/sqrt(x(i)**2+y(j)**2)
		end forall
		
		call figure()
		
		call subplot(1,1,1,is3d=.true.)
		call xyzlim(mixval(x),mixval(y),mixval(z),zoom=1.1_wp)
		call surface(x,y,z,11)
!~ 		call wireframe(x,y,z,lineColor='k')
		call box('x','y','z')
		
	end subroutine doSurface

end module examples_mod
