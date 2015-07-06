module plplotlib_mod
	!! Wrapper module for plplot to give it a more matplotlib like personality

	use kinds_mod
	use plplot
	implicit none
	private
	
	character(*),parameter::default_dev = 'qtwidget'
		!! Default output device
	
	!=================!
	!= Library State =!
	!=================!
	
	logical::isSetup = .false.
		!! Flag for library setup status
	logical::didShow = .false.
		!! Flag for library display status
	real(plflt)::fontScale = 1.0_plflt
		!! Font scale factor to resetPen
	logical::blackOnWhite = .true.
	
	!==============!
	!= Interfaces =!
	!==============!
	
	interface mixval
		!! Return a 2-vector comprising the minimum and maximum values of an array
		module procedure mixval_1
		module procedure mixval_2
		module procedure mixval_3
	end interface
	
	public::setup,show
	public::figure
	public::subplot
	public::xylim,xlim,ylim,xyzlim
	public::labels,xlabel,ylabel,title
	public::ticks,xticks,yticks,box
	public::legend
	
	public::mixval,linspace,binData
	
	public::plot
	public::scatter
	public::contour,contourf,colorbar
	public::bar,barh
	public::hist
	public::fillBetween,fillBetweenx
	public::quiver
	public::surface,wireframe
	
contains

	!===================!
	!= Helper Routines =!
	!===================!

	function mixval_1(x) result(b)
		!! Return [hi,low] for an array
		real(wp),dimension(:),intent(in)::x
			!! Array to find extrema in
		real(wp),dimension(2)::b
		
		b = [minval(x),maxval(x)]
	end function mixval_1

	function mixval_2(x) result(b)
		!! Return [hi,low] for an array
		real(wp),dimension(:,:),intent(in)::x
			!! Array to find extrema in
		real(wp),dimension(2)::b
		
		b = [minval(x),maxval(x)]
	end function mixval_2

	function mixval_3(x) result(b)
		!! Return [hi,low] for an array
		real(wp),dimension(:,:,:),intent(in)::x
			!! Array to find extrema in
		real(wp),dimension(2)::b
		
		b = [minval(x),maxval(x)]
	end function mixval_3

	function linspace(l,h,N) result(o)
		!! Return an array of evenly-spaced values
		real(wp),intent(in)::l
			!! Low-bound for values
		real(wp),intent(in)::h
			!! High-bound for values
		integer,intent(in),optional::N
			!! Number of values (default 20)
		real(wp),dimension(:),allocatable::o
		
		integer::Nl,i
		
		Nl = 20
		if(present(N)) Nl = N
		
		o = [( (h-l)*real(i-1,wp)/real(Nl-1,wp)+l , i=1 , Nl )]
	end function linspace

	function startsWith(text,str) result(o)
		!! Test if text starts with str
		character(*),intent(in)::text
			!! Text to search
		character(*),intent(in)::str
			!! String to look for
		logical::o
		integer::k
		
		k = len(str)
		o = text(1:k)==str
	end function startsWith

	function endsWith(text,str) result(o)
		!! Test if text ends with str
		character(*),intent(in)::text
			!! Text to search
		character(*),intent(in)::str
			!! String to look for
		logical::o
		integer::k
		
		k = len(text)
		o = text(k-len(str)+1:k)==str
	end function endsWith

	function binData(d,N,db,normalize) result(o)
		!! Count data in each bin
		real(wp),dimension(:),intent(in)::d
			!! Data for binning
		integer,intent(in),optional::N
			!! Number of bins
		real(wp),dimension(2),intent(in),optional::db
			!! Boundaries of bin range
		integer,intent(in),optional::normalize
			!! Normalization type (1=sum, 2=bin size, 3=maxval)
		real(wp),dimension(:,:),allocatable::o
		
		real(wp),dimension(:),allocatable::b
		integer::Nl,k
		
		Nl = 10
		if(present(N)) Nl = N
		
		if(present(db)) then
			b = linspace(db(1),db(2),Nl+1)
		else
			b = linspace(minval(d)-epsilon(1.0_wp),maxval(d)+epsilon(1.0_wp),Nl+1)
		end if
		
		allocate(o(Nl,2))
		o(:,1) = (b(1:Nl)+b(2:Nl+1))/2.0_wp
		
		do k=1,Nl
			o(k,2) = real(count(d>=b(k) .and. d<=b(k+1)),wp)
		end do
		
		if(present(normalize)) then
			select case(normalize)
			case(1)
				o(:,2) = o(:,2)/sum(o(:,2))
			case(2)
				do k=1,Nl
					o(k,2) = o(k,2)/(b(k+1)-b(k))
				end do
			case(3)
				o(:,2) = o(:,2)/maxval(o(:,2))
			end select
		end if
	end function binData

	!============================!
	!= Axes and Figure Routines =!
	!============================!

	subroutine figure
		!! Create a new figure
		logical,save::isFirst = .true.
		
		if(.not.isSetup) call setup()
		
		if(.not.isFirst) then
			call pleop()
		else
			isFirst = .false.
		end if
		
		call plbop()
		call plssub(1,1)
		call pladv(1)
		call resetPen
	end subroutine figure

	subroutine subplot(ny,nx,i,aspect,is3d)
		!! Create a set of axes on a figure
		integer,intent(in)::nx
			!! Number of subplot columns
		integer,intent(in)::ny
			!! Number of subplot rows
		integer,intent(in)::i
			!! Subplot to use
		real(wp),intent(in),optional::aspect
			!! Aspect ratio of the axes
		logical,intent(in),optional::is3d
		
		logical::is3dl
		
		call plssub(nx,ny)
		call pladv(i)
		call resetPen
		
		is3dl = .false.
		if(present(is3d)) is3dl = is3d
		
		if(is3dl) then
			call plvpor(0.0_wp,1.0_wp,0.0_wp,1.0_wp)
		else
			if(present(aspect)) then
				call plvasp(real(aspect,plflt))
			else
				call plvsta()
			end if
		end if
		
		call defaultLim
	end subroutine subplot

	subroutine defaultLim
		real(plflt),parameter::eps = epsilon(1.0_plflt)
		
		call plwind(-eps,eps,-eps,eps)
	end subroutine defaultLim

	subroutine xylim(xb,yb)
		!! Set the x and y ranges of the plot
		real(wp),dimension(2),intent(in)::xb
			!! x-range of plot
		real(wp),dimension(2),intent(in)::yb
			!! y-range of plot
		
		real(plflt),dimension(2)::xbl,ybl
		
		xbl = xb
		ybl = yb
		
		call plwind(xbl(1),xbl(2),ybl(1),ybl(2))
	end subroutine xylim

	subroutine xlim(xl,xh)
		!! Set the limits of the x-axis
		real(wp),intent(in)::xl,xh
		
		real(plflt)::x1,x2,y1,y2
		
		call plgvpw(x1,x2,y1,y2)
		call plwind(real(xl,plflt),real(xh,plflt),y1,y2)
	end subroutine xlim

	subroutine ylim(yl,yh)
		!! Set the limits of the y-axis
		real(wp),intent(in)::yl,yh
		
		real(plflt)::x1,x2,y1,y2
		
		call plgvpw(x1,x2,y1,y2)
		call plwind(x1,x2,real(yl,plflt),real(yh,plflt))
	end subroutine ylim

	subroutine xyzlim(xb,yb,zb,altitude,azimuth,zoom)
		!! Set the limits for a 3d plot
		real(wp),dimension(2),intent(in)::xb
			!! x-range of plot
		real(wp),dimension(2),intent(in)::yb
			!! y-range of plot
		real(wp),dimension(2),intent(in)::zb
			!! z-range of plot
		real(wp),intent(in),optional::altitude
			!! Altitude angle of plot in degrees
		real(wp),intent(in),optional::azimuth
			!! Azimuth angle of plot in degrees
		real(wp),intent(in),optional::zoom
			!! Zoom ratio (default 1.0)
		
		real(plflt)::al,az,zm
		
		al = 45.0_plflt
		if(present(altitude)) al = real(altitude,plflt)
		az = 60.0_plflt
		if(present(azimuth)) az = real(azimuth,plflt)
		zm = 1.0_plflt
		if(present(zoom)) zm = real(zoom,plflt)
		
		call plwind(-1.0_plflt,1.0_plflt,-1.0_plflt,1.5_plflt)
		call plw3d(zm,zm,1.2_plflt*zm, &
		& real(xb(1),plflt),real(xb(2),plflt), &
		& real(yb(1),plflt),real(yb(2),plflt), &
		& real(zb(1),plflt),real(zb(2),plflt),al,az)
	end subroutine xyzlim

	subroutine ticks(dx,dy,logx,logy,color,lineWidth)
		!! Set the ticks for the axes
		real(wp),intent(in),optional::dx
			!! Spacing between ticks on x-axis
		real(wp),intent(in),optional::dy
			!! Spacing between ticks on y-axis
		logical,intent(in),optional::logx
			!! Flag for log-ticks and labels on x-axis
		logical,intent(in),optional::logy
			!! Flag for log-ticks and labels on y-axis
		character(*),intent(in),optional::color
			!! Color code for ticks, box, and labels
		real(wp),optional::linewidth
			!! Line width for ticks and box
		
		real(plflt)::dxl,dyl
		character(10)::xopts,yopts
		
		dxl = 0.0_plflt
		if(present(dx)) dxl = real(dx,plflt)
		
		dyl = 0.0_plflt
		if(present(dy)) dyl = real(dy,plflt)
		
		xopts = 'bcnst'
		if(present(logx)) then
			if(logx) xopts = 'bcnstl'
		end if
		
		yopts = 'bcnstv'
		if(present(logy)) then
			if(logy) yopts = 'bcnstvl'
		end if
		
		call resetPen
		
		if(present(color)) call setColor(color)
		if(present(lineWidth)) call setLineWidth(lineWidth)
		
		call plbox(xopts,dxl,0,yopts,dyl,0)
		call resetPen
	end subroutine ticks

	subroutine box(xLabel,yLabel,zLabel,color)
		!! Set x,y and plot labels
		character(*),intent(in)::xLabel
			!! Label for x-axis
		character(*),intent(in)::yLabel
			!! Label for x-axis
		character(*),intent(in)::zLabel
			!! Label for z-axis
		character(*),intent(in),optional::color
			!! Color of labels
		
		if(present(color)) call setColor(color)
		call plbox3('bnstu',xLabel,0.0_wp,0,'bnstu',yLabel,0.0_wp,0,'bnstu',zLabel,0.0_wp,0)
		call resetPen
	end subroutine box

	subroutine xticks(d,logScale,primary,secondary,color,lineWidth)
		!! Set the ticks for the x-axis
		real(wp),intent(in),optional::d
			!! Spacing between ticks
		logical,intent(in),optional::logScale
			!! Flag for log-ticks and labels
		logical,intent(in),optional::primary
			!! Draw primary axis
		logical,intent(in),optional::secondary
			!! Draw secondary axis
		character(*),intent(in),optional::color
			!! Color code for ticks, box, and labels
		real(wp),optional::linewidth
			!! Line width for ticks and box
		real(plflt)::dxl,dyl
		character(10)::xopts,yopts
		
		dxl = 0.0_plflt
		dyl = 0.0_plflt
		if(present(d)) dxl = real(d,plflt)
		
		xopts = 'nst'
		
		if(present(primary)) then
			if(primary) xopts = trim(xopts)//'b'
		else
			xopts = trim(xopts)//'b'
		end if
		
		if(present(secondary)) then
			if(secondary) xopts = trim(xopts)//'c'
		else
			xopts = trim(xopts)//'c'
		end if
		
		if(present(logScale)) then
			if(logScale) xopts = trim(xopts)//'l'
		end if
		yopts = ''
		
		if(present(color)) call setColor(color)
		if(present(lineWidth)) call setLineWidth(lineWidth)
		call plbox(xopts,dxl,0,yopts,dyl,0)
		call resetPen
	end subroutine xticks

	subroutine yticks(d,logScale,primary,secondary,color,lineWidth)
		!! Set the ticks for the y-axis
		real(wp),intent(in),optional::d
			!! Spacing between ticks
		logical,intent(in),optional::logScale
			!! Flag for log-ticks and labels
		logical,intent(in),optional::primary
			!! Draw primary axis
		logical,intent(in),optional::secondary
			!! Draw secondary axis
		character(*),intent(in),optional::color
			!! Color code for ticks, box, and labels
		real(wp),optional::linewidth
			!! Line width for ticks and box
		real(plflt)::dxl,dyl
		character(10)::xopts,yopts
		
		dxl = 0.0_plflt
		dyl = 0.0_plflt
		if(present(d)) dyl = real(d,plflt)
		
		yopts = 'nst'
		
		if(present(primary)) then
			if(primary) yopts = trim(xopts)//'b'
		else
			yopts = trim(yopts)//'b'
		end if
		
		if(present(secondary)) then
			if(secondary) yopts = trim(yopts)//'c'
		else
			yopts = trim(yopts)//'c'
		end if
		
		if(present(logScale)) then
			if(logScale) yopts = trim(yopts)//'l'
		end if
		xopts = ''
		
		if(present(color)) call setColor(color)
		if(present(lineWidth)) call setLineWidth(lineWidth)
		call plbox(xopts,dxl,0,yopts,dyl,0)
		call resetPen
	end subroutine yticks

	subroutine labels(xLabel,yLabel,plotLabel,color)
		!! Set x,y and plot labels
		character(*),intent(in)::xLabel
			!! Label for x-axis
		character(*),intent(in)::yLabel
			!! Label for x-axis
		character(*),intent(in)::plotLabel
			!! Label entire plot
		character(*),intent(in),optional::color
			!! Color of labels
		
		if(present(color)) call setColor(color)
		call pllab(xLabel,yLabel,plotLabel)
		call resetPen
	end subroutine labels

	subroutine xlabel(label,color)
		!! Set x-label
		character(*),intent(in)::label
			!! Label for axis
		character(*),intent(in),optional::color
			!! Color of labels
		
		if(present(color)) call setColor(color)
		call plmtex('b',3.0_plflt,0.5_plflt,0.5_plflt,label)
		call resetPen
	end subroutine xlabel

	subroutine ylabel(label,color)
		!! Set y-label
		character(*),intent(in)::label
			!! Label for axis
		character(*),intent(in),optional::color
			!! Color of labels
		
		if(present(color)) call setColor(color)
		call plmtex('l',5.0_plflt,0.5_plflt,0.5_plflt,label)
		call resetPen
	end subroutine ylabel

	subroutine title(label,color)
		!! Set plot title
		character(*),intent(in)::label
			!! Label for plot
		character(*),intent(in),optional::color
			!! Color of labels
		
		if(present(color)) call setColor(color)
		call plmtex('t',1.5_plflt,0.5_plflt,0.5_plflt,label)
		call resetPen
	end subroutine title

	subroutine colorbar(z,N,leftLabel,rightLabel)
		!! Add a colorbar to the top of the plot
		real(wp),dimension(:,:),intent(in)::z
			!! Data used for levels computation
		integer,intent(in)::N
			!! Number of levels to compute
		character(*),intent(in),optional::leftLabel
			!! Label for left side of colorbar
		character(*),intent(in),optional::rightLabel
			!! Label for right side of colorbar
		
		real(plflt),dimension(:,:),allocatable::values
		character(64),dimension(2)::labels
		
		real(plflt)::fill_width
		real(plflt)::cont_width
		integer::cont_color
		real(plflt)::colorbar_width
		real(plflt)::colorbar_height
		integer::k
		
		values = reshape( &
			& real([( real(k-1,wp)/real(N-1,wp)*(maxval(z)-minval(z))+minval(z) ,k=1,N)],plflt), &
			& [N,1])
		
		fill_width = 2.0_plflt
		cont_width = 0.0_plflt
		cont_color = 1
		labels = ''
		if(present(leftLabel )) labels(1) = leftLabel
		if(present(rightLabel)) labels(2) = rightLabel
		
		call plcolorbar(colorbar_width,colorbar_height,&
			& ior(PL_COLORBAR_GRADIENT,PL_COLORBAR_SHADE_LABEL),PL_POSITION_TOP,&
			& 0.0_plflt,0.01_plflt,0.75_plflt,0.05_plflt,&
			& 0,1,1,0.0_plflt,0.0_plflt, &
			& cont_color,cont_width, &
			& [PL_COLORBAR_LABEL_LEFT,PL_COLORBAR_LABEL_RIGHT],labels, &
			& ['bcvmt'],[0.0_plflt],[0],[size(values)],values)
	end subroutine colorbar

	subroutine legend(corner,series,lineWidths,markScales,markCounts,ncol)
		!! Create legend for plot data
		!!
		!! FIXME: Text sizing should be modifiable
		character(*),intent(in)::corner
			!! Corner for legend
		character(*),dimension(:,:),intent(in)::series
			!! Data series in rows
			!! [name,textColor,lineStyle,lineColor,markStyle,markColor,boxColor]
		real(wp),dimension(:),intent(in),optional::lineWidths
			!! Line widths for the plots
		real(wp),dimension(:),intent(in),optional::markScales
			!! Marker sizes for the plots
		integer,dimension(:),intent(in),optional::markCounts
			!! Marker counts for the plots
		integer,intent(in),optional::ncol
			!! Number of columns
		
		real(plflt)::width,height,xoff,yoff
		real(plflt)::plotWidth
		integer::opt,cornerl
		integer::bg_color,bb_color,bb_style,lncol,lnrow
		integer,dimension(size(series,1))::opts
		real(plflt),dimension(size(series,1))::lwidths,mscales
		integer,dimension(size(series,1))::mcounts,text_colors
		real(plflt)::text_offset,text_scale,text_spacing,text_justification
		integer,dimension(size(series,1))::box_colors,box_patterns
		real(plflt),dimension(size(series,1))::box_scales,box_line_widths
		integer,dimension(size(series,1))::line_colors,line_styles
		integer,dimension(size(series,1))::mark_colors
		character(64),dimension(size(series,1))::mark_styles
		integer::k
		
		call doLegendBox
		
		opts = 0
		do k=1,size(series,1)
			if(series(k,3)/='') opts(k) = ior(opts(k),PL_LEGEND_LINE)
			if(series(k,5)/='') opts(k) = ior(opts(k),PL_LEGEND_SYMBOL)
			if(series(k,7)/='') opts(k) = ior(opts(k),PL_LEGEND_COLOR_BOX)
		end do
		
		call doText
		call doBoxes
		call doLines
		call doMarkers
		
		call pllegend(width,height,opt,cornerl,xoff,yoff,plotWidth, &
			& bg_color,bb_color,bb_style, &
			& lnrow,lncol,size(series,1),opts,text_offset, &
			& text_scale,text_spacing,text_justification,text_colors,series(:,1), &
			& box_colors,box_patterns,box_scales,box_line_widths, &
			& line_colors,line_styles,lwidths, &
			& mark_colors,mscales,mcounts,mark_styles)
		
	contains
	
		subroutine doLegendBox
			opt = PL_LEGEND_BACKGROUND+PL_LEGEND_BOUNDING_BOX
			cornerl = getCorner(corner)
			xoff = 0.0_plflt
			yoff = 0.0_plflt
			plotWidth = 0.05_plflt
			bg_color = 0
			bb_color = 1
			bb_style = getLineStyleCode('-')
			
			lncol = 1
			if(present(ncol)) lncol = ncol
			lnrow = size(series,1)/lncol
		end subroutine doLegendBox
	
		subroutine doText
			text_offset  = 0.3_plflt
			text_scale   = fontScale
			text_spacing = 3.0_plflt
			text_justification = 0.0_plflt
			
			do k=1,size(series,1)
				text_colors = getColorCode(series(k,2))
			end do
		end subroutine doText
	
		subroutine doBoxes
			do k=1,size(series,1)
				box_colors(k) = getColorCode(series(k,7))
			end do
			box_patterns = 0
			box_scales = 0.5_plflt
			box_line_widths = 0.0_plflt
		end subroutine doBoxes
	
		subroutine doLines
			lwidths = 1.0_plflt
			if(present(lineWidths)) lwidths = real(lineWidths,plflt)
			
			do k=1,size(series,1)
				line_colors(k) = getColorCode(series(k,4))
				line_styles(k) = getLineStyleCode(series(k,3))
			end do
		end subroutine doLines
	
		subroutine doMarkers
			mcounts = 2
			if(present(markCounts)) mcounts = markCounts
			mscales = 1.0_plflt
			if(present(markScales)) mscales = real(markScales,plflt)
			
			do k=1,size(series,1)
				mark_colors(k) = getColorCode(series(k,6))
				mark_styles(k) = getSymbolCode(series(k,5))
			end do
		end subroutine doMarkers
	
		function getCorner(text) result(code)
			character(*),intent(in)::text
			integer::code
			
			code = PL_POSITION_INSIDE
			if( startsWith(text,'upper') ) code = code+PL_POSITION_TOP
			if( startsWith(text,'lower') ) code = code+PL_POSITION_BOTTOM
			if(   endsWith(text,'right') ) code = code+PL_POSITION_RIGHT
			if(   endsWith(text,'left' ) ) code = code+PL_POSITION_LEFT
		end function getCorner
	
	end subroutine legend

	!=====================!
	!= Plotting Routines =!
	!=====================!

	subroutine hist(d,N,db,relWidth,fillColor,fillPattern,lineColor,lineWidth)
		!! Create a histogram
		real(wp),dimension(:),intent(in)::d
			!! Data for binning
		integer,intent(in),optional::N
			!! Number of bins
		real(wp),dimension(2),intent(in),optional::db
			!! Boundaries of bin range
		real(wp),intent(in),optional::relWidth
			!! Relative width of bars (default 0.8)
		character(*),intent(in),optional::fillColor
			!! Color of bar fills
		character(*),intent(in),optional::fillPattern
			!! Pattern of bar fills
		character(*),intent(in),optional::lineColor
			!! Color of lines around bars
		real(wp),optional::lineWidth
			!! Width of lines around bars
		
		real(wp),dimension(:,:),allocatable::h
		real(wp),dimension(2)::dbl
		integer::Nl
		
		real(wp)::relWidthl
		real(wp)::lineWidthl
		
		Nl = 20
		if(present(N)) Nl = N
		
		if(present(db)) then
			dbl = db
		else
			dbl = mixval(d)+[-1.0_wp,1.0_wp]*epsilon(1.0_wp)
		end if
		
		h = binData(d,Nl,dbl,normalize=3)
		
		relWidthl = 1.0_wp
		if(present(relWidth)) relWidthl = relWidth
		lineWidthl = 0.5_wp
		if(present(lineWidth)) lineWidthl = lineWidth
		
		if(present(lineColor)) then
			if(present(fillColor)) then
				if(present(fillPattern)) then
					call bar(h(:,1),h(:,2),relWidth=relWidthl,lineColor=lineColor,lineWidth=lineWidthl, &
					& fillColor=fillColor,fillPattern=fillPattern)
				else
					call bar(h(:,1),h(:,2),relWidth=relWidthl,lineColor=lineColor,lineWidth=lineWidthl, &
					& fillColor=fillColor)
				end if
			else
				if(present(fillPattern)) then
					call bar(h(:,1),h(:,2),h(:,2),relWidth=relWidthl,lineColor=lineColor,lineWidth=lineWidthl, &
					& fillPattern=fillPattern)
				else
					call bar(h(:,1),h(:,2),h(:,2),relWidth=relWidthl,lineColor=lineColor,lineWidth=lineWidthl)
				end if
			end if
		else
			if(present(fillColor)) then
				if(present(fillPattern)) then
					call bar(h(:,1),h(:,2),relWidth=relWidthl,lineWidth=lineWidthl, &
					& fillColor=fillColor,fillPattern=fillPattern)
				else
					call bar(h(:,1),h(:,2),relWidth=relWidthl,lineWidth=lineWidthl, &
					& fillColor=fillColor)
				end if
			else
				if(present(fillPattern)) then
					call bar(h(:,1),h(:,2),h(:,2),relWidth=relWidthl,lineWidth=lineWidthl, &
					& fillPattern=fillPattern)
				else
					call bar(h(:,1),h(:,2),h(:,2),relWidth=relWidthl,lineWidth=lineWidthl)
				end if
			end if
		end if

		
		call resetPen
	end subroutine hist

	subroutine scatter(x,y,c,s,markColor,markStyle,markSize)
		!! Create scatter plot of data
		real(wp),dimension(:),intent(in)::x
			!! x-coordinates of data
		real(wp),dimension(:),intent(in)::y
			!! y-coordinates of data
		real(wp),dimension(:),intent(in),optional::c
			!! Data for smooth coloring
		real(wp),dimension(:),intent(in),optional::s
			!! Data for marker scaling
		character(*),intent(in),optional::markColor
			!! Color of markers; overridden by z
		character(*),intent(in),optional::markStyle
			!! Style of markers
		real(wp),intent(in),optional::markSize
			!! Size of markers
		
		real(plflt),dimension(:),allocatable::xl,yl
		real(plflt),dimension(:),allocatable::cb
		character(32)::code
		integer::k
		
		xl = x
		yl = y
		
		if(present(markColor)) call setColor(markColor)
		code = getSymbolCode('')
		if(present(markStyle)) code = getSymbolCode(markStyle)
		if(present(markSize)) call plschr(0.0_plflt,real(markSize,plflt))
		if(present(markSize)) call plssym(0.0_plflt,real(markSize,plflt))
		
		if(present(c)) cb = real(mixval(c),plflt)
		do k=1,size(x)
			if(present(c)) call plcol1( real( (c(k)-cb(1))/(cb(2)-cb(1)) ,plflt) )
			if(present(s)) call plschr(0.0_plflt,real(s(k),plflt))
			if(present(s)) call plssym(0.0_plflt,real(s(k),plflt))
			call plptex(xl(k),yl(k),0.0_plflt,0.0_plflt,0.5_plflt,code)
		end do
		call resetPen
	end subroutine scatter

	subroutine plot(x,y,lineColor,lineStyle,lineWidth,markColor,markStyle,markSize)
		!! Plot data using lines and or markers
		real(wp),dimension(:),intent(in)::x
			!! x-data for plot
		real(wp),dimension(:),intent(in)::y
			!! y-data for plot
		character(*),intent(in),optional::lineColor
			!! Color of line
		character(*),intent(in),optional::lineStyle
			!! Style of line; '' for no line
		real(wp),intent(in),optional::lineWidth
			!! Width of line
		character(*),intent(in),optional::markColor
			!! Color of markers, if any
		character(*),intent(in),optional::markStyle
			!! Style of markers; '' or absent for none
		real(wp),intent(in),optional::markSize
			!! Size of markers, if any
		
		real(plflt),dimension(:),allocatable::xl,yl
		character(32)::code
		integer::k
		
		xl = x
		yl = y
		
		if(present(lineColor)) call setColor(lineColor)
		if(present(lineWidth)) call setLineWidth(lineWidth)
		if(present(lineStyle)) then
			call setLineStyle(lineStyle)
			if(lineStyle/='') call plline(xl,yl)
		else
			call plline(xl,yl)
		end if
		call resetPen
		
		if(present(markColor)) call setColor(markColor)
		if(present(markSize)) call plssym(0.0_plflt,real(markSize,plflt))
		if(present(markStyle)) then
			code = getSymbolCode(markStyle)
			if(markStyle/='') then
				do k=1,size(x)
					call plptex(xl(k),yl(k),0.0_plflt,0.0_plflt,0.5_plflt,code)
				end do
			end if
		end if
		call resetPen
	end subroutine plot

	subroutine contour(x,y,z,N,lineColor,lineStyle,lineWidth)
		!! Plot contour lines
		real(wp),dimension(:),intent(in)::x
			!! x-coordinates of data
		real(wp),dimension(:),intent(in)::y
			!! y-coordinates of data
		real(wp),dimension(:,:),intent(in)::z
			!! Data for contouring
		integer,intent(in),optional::N
			!! Number of levels to use in contour
		character(*),intent(in),optional::lineColor
			!! Color of contour lines
		character(*),intent(in),optional::lineStyle
			!! Style of contour lines
		real(wp),optional::lineWidth
			!! Width of contour lines
		
		real(plflt),dimension(:),allocatable::xl,yl
		real(plflt),dimension(:,:),allocatable::zl
		
		real(plflt),dimension(:),allocatable::edge
		integer::Nl,k
		
		xl = x
		yl = y
		zl = z
		Nl = 20
		if(present(N)) Nl = N
		edge = [( real(k-1,plflt)/real(Nl-1,plflt)*(maxval(zl)-minval(zl))+minval(zl) ,k=1,Nl)]
		
		if(present(lineColor)) call setColor(lineColor)
		if(present(lineStyle)) call setLineStyle(lineStyle)
		if(present(lineWidth)) call setLineWidth(lineWidth)
		
		call plcont(zl,edge,xl,yl)
		call resetPen
	end subroutine contour

	subroutine surface(x,y,z,N,lineStyle)
		!! Plot a 3d surface
		real(wp),dimension(:),intent(in)::x
			!! x-coordinates of data
		real(wp),dimension(:),intent(in)::y
			!! y-coordinates of data
		real(wp),dimension(:,:),intent(in)::z
			!! Data for contouring
		integer,intent(in),optional::N
			!! Number of levels to use in surface colors
		character(*),intent(in),optional::lineStyle
			!! Style for xy lines ( '-' = on, '' = off )
		
		real(plflt),dimension(:),allocatable::xl,yl
		real(plflt),dimension(:,:),allocatable::zl
		
		real(plflt),dimension(:),allocatable::edge
		integer::Nl,opt
		
		opt = MAG_COLOR
		
		xl = x
		yl = y
		zl = z
		Nl = 20
		if(present(N)) then
			Nl = N
			opt = ior(opt,SURF_CONT)
		end if
		edge = linspace(minval(z),maxval(z),Nl)
		
		if(present(lineStyle)) then
			select case(lineStyle)
			case('')
				opt = opt
			case('-')
				opt = ior(opt,FACETED)
			end select
		end if
		
		call plsurf3d(xl,yl,zl,opt,edge)
		call resetPen
	end subroutine surface

	subroutine wireframe(x,y,z,lineColor)
		!! Plot a 3d wireframe
		real(wp),dimension(:),intent(in)::x
			!! x-coordinates of data
		real(wp),dimension(:),intent(in)::y
			!! y-coordinates of data
		real(wp),dimension(:,:),intent(in)::z
			!! Data for contouring
		character(*),intent(in),optional::lineColor
			!! Color of contour lines
		
		real(plflt),dimension(:),allocatable::xl,yl
		real(plflt),dimension(:,:),allocatable::zl
		
		xl = x
		yl = y
		zl = z
		
		if(present(lineColor)) then
			call setColor(lineColor)
			call plot3d(xl,yl,zl,DRAW_LINEXY,.false.)
		else
			call plot3d(xl,yl,zl,ior(DRAW_LINEXY,MAG_COLOR),.false.)
		end if
		
		call resetPen
	end subroutine wireframe

	subroutine contourf(x,y,z,N)
		!! Plot filled contours
		real(wp),dimension(:),intent(in)::x
			!! x-coordinates of data
		real(wp),dimension(:),intent(in)::y
			!! y-coordinates of data
		real(wp),dimension(:,:),intent(in)::z
			!! Data for contouring
		integer,intent(in),optional::N
			!! Number of levels to use in contour
		
		real(plflt),dimension(:),allocatable::xl,yl
		real(plflt),dimension(:,:),allocatable::zl
		
		real(plflt),dimension(:),allocatable::edge
		
		character(1)::defined
		real(plflt)::fill_width
		real(plflt)::cont_width
		integer::cont_color
		integer::Nl
		
		xl = x
		yl = y
		zl = z
		Nl = 20
		if(present(N)) Nl = N
		
		edge = linspace(minval(z),maxval(z),Nl)
		
		fill_width = -1.0_plflt
		cont_width = -1.0_plflt
		cont_color = -1
		
		call plshades(zl,defined,minval(xl),maxval(xl),minval(yl),maxval(yl), &
			& edge,fill_width,cont_color,cont_width)
		call resetPen
	end subroutine contourf

	subroutine quiver(x,y,u,v,s,c,scaling,lineColor,lineStyle,lineWidth)
		!! Plot vectors
		real(wp),dimension(:),intent(in)::x
			!! x-positions of vectors
		real(wp),dimension(:),intent(in)::y
			!! y-positions of vectors
		real(wp),dimension(:,:),intent(in)::u
			!! u-components of vectors
		real(wp),dimension(:,:),intent(in)::v
			!! v-components of vectors
		real(wp),dimension(:,:),intent(in),optional::s
			!! Scale of vectors
		real(wp),dimension(:,:),intent(in),optional::c
			!! Color values for vectors
		real(wp),intent(in),optional::scaling
			!! Scaling of vectors
			!! < 0 = Automatic, then scaled
			!!   0 = Automatic
			!! > 0 = Directly scaled
		character(*),intent(in),optional::lineColor
			!! Color of vectors
		character(*),intent(in),optional::lineStyle
			!! Style of vectors' lines
		real(wp),optional::lineWidth
			!! Width of vectors' lines
		
		real(plflt),dimension(:),allocatable::xl,yl
		real(plflt),dimension(:,:),allocatable::ul,vl,sl
		real(plflt),dimension(2)::xb,yb,sb,cb,d
		real(plflt)::scalingl,scl,mag
		integer::i,j
		
		xl = x
		yl = y
		ul = u
		vl = v
		
		d = real([x(2)-x(1),y(2)-y(1)],plflt)
		
		xb = real(mixval(x),plflt)
		yb = real(mixval(y),plflt)
		if(present(s)) then
			sl = s
			sl = sl/maxval(sl)
		else
			sl = u**2+v**2
			sl = sqrt(sl)
			sl = sl/maxval(sl)
		end if
		sb = [minval(sl),maxval(sl)]
		if(present(c)) cb = real([minval(c),maxval(c)],plflt)
		
		scalingl = 1.0_plflt
		if(present(scaling)) scalingl = real(scaling,plflt)
		
		if(present(lineColor)) call setColor(lineColor)
		if(present(lineStyle)) call setLineStyle(lineStyle)
		if(present(lineWidth)) call setLineWidth(lineWidth)
		
		do i=1,size(u,1)
			do j=1,size(u,2)
				mag = norm2([ul(i,j),vl(i,j)])
				scl = scalingl*norm2(d)*sl(i,j)
				if(abs(scl)<1.0E-5_wp) cycle
				if(present(c)) call plcol1( real( (c(i,j)-cb(1))/(cb(2)-cb(1)) ,plflt) )
				call plvect(ul(i:i,j:j)/mag,vl(i:i,j:j)/mag,scl,xl(i:i),yl(j:j))
			end do
		end do
		
		call resetPen
	end subroutine quiver

	subroutine bar(x,y,c,relWidth,fillColor,fillPattern,lineColor,lineWidth)
		!! Create a bar graph
		real(wp),dimension(:),intent(in)::x
			!! x-positions of the bars' centers
		real(wp),dimension(:),intent(in)::y
			!! y-positions of the bars' tops
		real(wp),dimension(:),intent(in),optional::c
			!! Color scale for bars
		real(wp),intent(in),optional::relWidth
			!! Relative width of bars (default 0.8)
		character(*),intent(in),optional::fillColor
			!! Color of bar fills
		character(*),intent(in),optional::fillPattern
			!! Pattern of bar fills
		character(*),intent(in),optional::lineColor
			!! Color of lines around bars
		real(wp),optional::lineWidth
			!! Width of lines around bars
		
		real(plflt),dimension(4)::xl,yl
		real(plflt),dimension(2)::cb
		real(plflt)::dx,dxs
		integer::k
		
		if(present(c)) cb = real(mixval(c),plflt)
		dxs = 0.8_plflt
		if(present(relWidth)) dxs = real(relWidth,plflt)
		if(size(x)>1) then
			dx = dxs*real(x(2)-x(1),plflt)/2.0_plflt
		else
			dx = dxs
		end if
		
		if(present(lineWidth)) call setLineWidth(lineWidth)
		
		do k=1,size(x)
			xl = real([x(k)-dx,x(k)-dx,x(k)+dx,x(k)+dx],plflt)
			yl = real([0.0_wp,y(k),y(k),0.0_wp],plflt)
			
			if(present(fillColor)) call setColor(fillColor)
			if(present(fillPattern)) call setFillPattern(fillPattern)
			if(present(c)) call plcol1( real( (c(k)-cb(1))/(cb(2)-cb(1)) ,plflt) )
			call plfill(xl,yl)
			
			if(present(lineColor)) call setColor(lineColor)
			call plline(xl,yl)
		end do
		call resetPen
	end subroutine bar

	subroutine barh(y,x,c,relWidth,fillColor,fillPattern,lineColor,lineWidth)
		!! Create a horizontal bar graph
		real(wp),dimension(:),intent(in)::y
			!! y-positions of the bars' centers
		real(wp),dimension(:),intent(in)::x
			!! x-positions of the bars' tops
		real(wp),dimension(:),intent(in),optional::c
			!! Color scale for bars
		real(wp),intent(in),optional::relWidth
			!! Relative width of bars
		character(*),intent(in),optional::fillColor
			!! Color of bar fills
		character(*),intent(in),optional::fillPattern
			!! Pattern of bar fills
		character(*),intent(in),optional::lineColor
			!! Color of lines around bars
		real(wp),optional::lineWidth
			!! Width of lines around bars
		
		real(plflt),dimension(4)::xl,yl
		real(plflt),dimension(2)::cb
		real(plflt)::dy,dys
		integer::k
		
		if(present(c)) cb = real(mixval(c),plflt)
		dys = 0.8_plflt
		if(present(relWidth)) dys = real(relWidth,plflt)
		dy = dys*real(y(2)-y(1),plflt)/2.0_plflt
		
		if(present(lineWidth)) call setLineWidth(lineWidth)
		
		do k=1,size(x)
			yl = real([y(k)-dy,y(k)-dy,y(k)+dy,y(k)+dy],plflt)
			xl = real([0.0_wp,x(k),x(k),0.0_wp],plflt)
			
			if(present(fillColor)) call setColor(fillColor)
			if(present(fillPattern)) call setFillPattern(fillPattern)
			if(present(c)) call plcol1( real( (c(k)-cb(1))/(cb(2)-cb(1)) ,plflt) )
			call plfill(xl,yl)
			
			if(present(lineColor)) call setColor(lineColor)
			call plline(xl,yl)
		end do
		call resetPen
	end subroutine barh

	subroutine fillBetween(x,y1,y0,fillColor,fillPattern,lineWidth)
		!! Fill space between two lines
		real(wp),dimension(:),intent(in)::x
		real(wp),dimension(:),intent(in)::y1
		real(wp),dimension(:),intent(in),optional::y0
		character(*),intent(in),optional::fillColor
		character(*),intent(in),optional::fillPattern
		real(wp),intent(in),optional::lineWidth
		
		real(plflt),dimension(:),allocatable::xl,y1l,y0l
		integer::N
		
		N = size(x)
		
		xl  = x
		y1l = y1
		if(present(y0)) then
			y0l = y0
		else
			allocate(y0l(N))
			y0l = 0.0_plflt
		end if
		
		if(present(fillColor)) call setColor(fillColor)
		if(present(fillPattern)) call setFillPattern(fillPattern)
		if(present(lineWidth)) call setLineWidth(lineWidth)
		call plfill([xl(1:N:1),xl(N:1:-1)],[y1l(1:N:1),y0l(N:1:-1)])
		call resetPen
	end subroutine fillBetween

	subroutine fillBetweenx(y,x1,x0,fillColor,fillPattern,lineWidth)
		!! Fill space between two lines
		real(wp),dimension(:),intent(in)::y
		real(wp),dimension(:),intent(in)::x1
		real(wp),dimension(:),intent(in),optional::x0
		character(*),intent(in),optional::fillColor
		character(*),intent(in),optional::fillPattern
		real(wp),intent(in),optional::lineWidth
		
		real(plflt),dimension(:),allocatable::yl,x1l,x0l
		integer::N
		
		N = size(y)
		
		yl  = y
		x1l = x1
		if(present(x0)) then
			x0l = x0
		else
			allocate(x0l(N))
			x0l = 0.0_plflt
		end if
		
		if(present(fillColor)) call setColor(fillColor)
		if(present(fillPattern)) call setFillPattern(fillPattern)
		if(present(lineWidth)) call setLineWidth(lineWidth)
		call plfill([x1l(1:N:1),x0l(N:1:-1)],[yl(1:N:1),yl(N:1:-1)])
		call resetPen
	end subroutine fillBetweenx

	!========================!
	!= Drawing Pen Routines =!
	!========================!

	subroutine resetPen
		!! Reset pen to default state
		
		call setColor('')
		call setLineStyle('')
		call setLineWidth(0.5_wp)
		call setFillPattern('')
		call plschr(0.0_plflt,real(fontScale,plflt))
		call plssym(0.0_plflt,real(fontScale,plflt))
	end subroutine resetPen

	subroutine setLineWidth(lineWidth)
		real(wp),intent(in)::lineWidth
		
		call plwidth(real(lineWidth,plflt))
	end subroutine setLineWidth

	subroutine setLineStyle(style)
		!! Set the current pen line style
		character(*),intent(in)::style
			!! Style to set
		
		call pllsty(getLineStyleCode(style))
	end subroutine setLineStyle

	function getLineStyleCode(style) result(code)
		!! Return the code for a line style
		character(*),intent(in)::style
			!! Style desired
		integer::code
		
		select case(style)
		case('-')
			code = 1
		case(':')
			code = 2
		case('--')
			code = 3
		case default
			code = 1
		end select
	end function getLineStyleCode

	function getSymbolCode(style) result(code)
		!! Return the code for a symbol style
		character(*),intent(in)::style
			!! Style desired
		character(32)::code
		
		select case(style)
		case('+')
			code = '#(140)'
		case('x')
			code = '#(141)'
		case('*')
			code = '#(142)'
		case('.')
			code = '#(143)'
		case('s')
			code = '#(144)'
		case(',')
			code = '#(850)'
		case('^')
			code = '#(852)'
		case('<')
			code = '#(853)'
		case('v')
			code = '#(854)'
		case('>')
			code = '#(855)'
		case default
			code = '#(143)'
		end select
	end function getSymbolCode

	subroutine setFillPattern(style)
		character(*),intent(in)::style
		
		call plpsty(getFillCode(style))
	end subroutine setFillPattern

	function getFillCode(style) result(code)
		character(*),intent(in)::style
		integer::code
		
		select case(style)
		case('')
			code = 0
		case('-')
			code = 1
		case('/')
			code = 3
		case('|')
			code = 2
		case('\')
			code = 4
		case('#')
			code = 7
		case('x')
			code = 8
		case default
			code = 0
		end select
	end function getFillCode

	subroutine setColor(color)
		!! Set the current pen color
		character(*),intent(in)::color
			!! Name of color to set
		
		integer::ios
		real(plflt)::v
		
		read(color,*,iostat=ios) v
		if(ios==0) then
			call plcol1(v)
		else
			call plcol0(getColorCode(color))
		end if
	end subroutine setColor

	function getColorCode(color) result(code)
		character(*),intent(in)::color
		integer::code
		
		select case(color)
		case('w','white')
			if(blackOnWhite) then
				code = 1
			else
				code = 2
			end if
		case('k','black')
			if(blackOnWhite) then
				code = 2
			else
				code = 1
			end if
		case('r','red')
			code = 3
		case('g','green')
			code = 4
		case('b','blue')
			code= 5
		case('c','cyan')
			code = 6
		case('m','magenta')
			code= 7
		case('y','yellow')
			code = 8
		case('fg')
			code = 2
		case('bg')
			code = 1
		case default
			code = 2
		end select
		
		code = code-1
	end function getColorCode

	!===========================!
	!= Library Status Routines =!
	!===========================!

	subroutine setup(device,fileName,fontScaling,whiteOnBlack,colormap,figSize)
		!! Setup PlPlot library, optionally overriding defaults
		character(*),intent(in),optional::device
			!! Output device to use
		character(*),intent(in),optional::fileName
			!! Name of file(s) to write to
			!! %n will be replaced with the figure number
		real(wp),intent(in),optional::fontScaling
			!! Font scaling relative to default value
		logical,intent(in),optional::whiteOnBlack
			!! Default foreground and background colors
		character(*),intent(in),optional::colormap
			!! Colormap to use
		integer,dimension(2),intent(in),optional::figSize
			!! Size of figures to produce in pixels
		
		character(64)::bufx,bufy
		
		if(isSetup) return
		isSetup = .true.
		
		if(present(device)) then
			call plsdev(device)
		else
			call plsdev(default_dev)
		end if
		
		call plsfam(1,1,100)
		if(present(fileName)) then
			call plsfnam(fileName)
		else
			call plsfnam('out')
		end if
		
		if(present(whiteOnBlack)) blackOnWhite = .not. whiteOnBlack
		
		call setIndexedColors
		
		if(present(colormap)) then
			call setColormap(colormap)
		else
			call setColormap('CoolWarm')
		end if
		
		call plfontld(0)
		if(present(fontScaling)) fontScale = real(fontScaling,plflt)
		
		if(present(figSize)) then
			write(bufx,*) figSize(1)
			write(bufy,*) figSize(2)
			call plsetopt('geometry',trim(adjustl(bufx))//'x'//trim(adjustl(bufy)))
		else
			call plsetopt('geometry','640x480')
		end if
		
		call plinit
		
		call resetPen
	end subroutine setup

	subroutine show
		!! Show the plots end finialize the PlPlot library
		if(.not.didShow) then
			call plend
			didShow = .true.
		end if
	end subroutine show

	!======================!
	!= Color Map Routines =!
	!======================!

	subroutine setIndexedColors
		!! Setup the indexed colors
		integer,dimension(8,3)::rgb
		
		rgb(getColorCode('w')+1,:) = [255,255,255] ! White
		rgb(getColorCode('k')+1,:) = [  0,  0,  0] ! Black
		rgb(getColorCode('r')+1,:) = [255,  0,  0] ! Red
		rgb(getColorCode('g')+1,:) = [  0,255,  0] ! Green
		rgb(getColorCode('b')+1,:) = [  0,  0,255] ! Blue
		rgb(getColorCode('c')+1,:) = [  0,255,255] ! Cyan
		rgb(getColorCode('m')+1,:) = [255,  0,255] ! Magenta
		rgb(getColorCode('y')+1,:) = [255,255,  0] ! Yellow
		
		call plscmap0(rgb(:,1),rgb(:,2),rgb(:,3))
	end subroutine setIndexedColors

	subroutine setColormap(colormap)
		!! Set the continuous colormap
		character(*),intent(in)::colormap
			!! Name of colormap to use
		
		real(plflt),dimension(:),allocatable::i,h,s,v
		
		select case(colormap)
		case('CoolWarm')
			h = [240.0,195.0,45.0,0.0]
			
			s = [0.60, 0.95, 0.95, 0.60]
			v = [0.80, 0.30, 0.30, 0.80]
			i = [0.00, 0.50, 0.50, 1.00]
			
			call plscmap1n(256)
			call plscmap1l(.false.,i,h,s,v)
		case('Gray')
			call plspal1('cmap1_gray.pal',1)
		case('BlueYellow')
			call plspal1('cmap1_blue_yellow.pal',1)
		case('BlueRed')
			call plspal1('cmap1_blue_red.pal',1)
		case('Radar')
			call plspal1('cmap1_radar.pal',1)
		case('HighFreq')
			call plspal1('cmap1_highfreq.pal',1)
		case('LowFreq')
			call plspal1('cmap1_lowfreq.pal',1)
		end select
	end subroutine setColormap

end module plplotlib_mod
