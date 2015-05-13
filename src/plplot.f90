module plplot_mod
	!! Wrapper module for plplot to give it a more matplotlib like personality
	use kinds_mod
	use plplot
	implicit none
	public
	
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
	
contains

	!===================!
	!= Helper Routines =!
	!===================!

	function mixval(x) result(b)
		!! Return [hi,low] for an array
		!! FIXME: Implement as generic function for arrays 1d,2d,3d
		real(wp),dimension(:),intent(in)::x
			!! Array to find extrema in
		real(wp),dimension(2)::b
		
		b = [minval(x),maxval(x)]
	end function mixval

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

	!============================!
	!= Axes and Figure Routines =!
	!============================!

	subroutine figure
		!! Create a new figure
		if(.not.isSetup) call setup()
		
		call pladv(0)
	end subroutine figure

	subroutine subplot(ny,nx,i,aspect)
		!! Create a set of axes on a figure
		integer,intent(in)::nx
			!! Number of subplot columns
		integer,intent(in)::ny
			!! Number of subplot rows
		integer,intent(in)::i
			!! Subplot to use
		real(wp),intent(in),optional::aspect
			!! Aspect ratio of the axes
		
		call plssub(nx,ny)
		call pladv(i)
		call resetPen
		
		if(present(aspect)) then
			call plvasp(real(aspect,plflt))
		else
			call plvsta()
		end if
	end subroutine subplot

	subroutine xylim(xb,yb)
		!! Set the x and y ranges of the plot
		real(wp),dimension(2),intent(in)::xb
			!! x-range of plot
		real(wp),dimension(2),intent(in)::yb
			!! y-range of plot
		
		call plwind(xb(1),xb(2),yb(1),yb(2))
		call resetPen
	end subroutine xylim

	subroutine ticks(dx,dy,logx,logy,color,linewidth)
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
		if(present(dx)) dxl = dx
		
		dyl = 0.0_plflt
		if(present(dy)) dyl = dy
		
		xopts = 'bcnst'
		if(present(logx)) then
			if(logx) xopts = 'bcnstl'
		end if
		
		yopts = 'bcnstv'
		if(present(logy)) then
			if(logy) yopts = 'bcnstvl'
		end if
		
		if(present(color)) call setColor(color)
		if(present(linewidth)) call setLineWidth(lineWidth)
		
		call plbox(xopts,dxl,0,yopts,dyl,0)
		call resetPen
	end subroutine ticks

	subroutine labels(xLabel,yLabel,plotLabel,color)
		!! Set x,y and plot labels
		character(*),intent(in)::xLabel
			!! Label for x-axis
		character(*),intent(in)::yLabel
			!! Label for x-axis
		character(*),intent(in)::plotLabel
			!! Label entire plot
		character(*),intent(in),optional::color
		
		if(present(color)) call setColor(color)
		call pllab(xLabel,yLabel,plotLabel)
		call resetPen
	end subroutine labels

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
			& [( real(k-1,plflt)/real(N-1,plflt)*(maxval(z)-minval(z))+minval(z) ,k=1,N)], &
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
		!! Create legend for plot data.  
		!! ![Example-1](../media/example-4.svg)
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
			plotWidth = 0.05_wp
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
			if(present(lineWidths)) lwidths = lineWidths
			
			do k=1,size(series,1)
				line_colors(k) = getColorCode(series(k,4))
				line_styles(k) = getLineStyleCode(series(k,3))
			end do
		end subroutine doLines
	
		subroutine doMarkers
			mcounts = 2
			if(present(markCounts)) mcounts = markCounts
			mscales = 1.0_plflt
			if(present(markScales)) mscales = markScales
			
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

	subroutine scatter(x,y,c,s,markColor,markStyle,markSize)
		!! Create scatter plot of data.  
		!! ![Example-1](../media/example-2.svg)
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
		
		if(present(c)) cb = mixval(c)
		do k=1,size(x)
			if(present(c)) call plcol1( (c(k)-cb(1))/(cb(2)-cb(1)) )
			if(present(s)) call plschr(0.0_plflt,s(k))
			if(present(s)) call plssym(0.0_plflt,s(k))
			call plptex(x(k),y(k),0.0_plflt,0.0_plflt,0.5_plflt,code)
		end do
		call resetPen
	end subroutine scatter

	subroutine plot(x,y,lineColor,lineStyle,lineWidth,markColor,markStyle,markSize)
		!! Plot data using lines and or markers.  
		!! ![Example-1](../media/example-1.svg)
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
			if(lineStyle/='') call plline(x,y)
		else
			call plline(x,y)
		end if
		call resetPen
		
		if(present(markColor)) call setColor(markColor)
		if(present(markSize)) call plssym(0.0_plflt,real(markSize,plflt))
		if(present(markStyle)) then
			code = getSymbolCode(markStyle)
			if(markStyle/='') then
				do k=1,size(x)
					call plptex(x(k),y(k),0.0_plflt,0.0_plflt,0.5_plflt,code)
				end do
			end if
		end if
		call resetPen
	end subroutine plot

	subroutine contour(x,y,z,N,lineColor,lineStyle,lineWidth)
		!! Plot contour lines.  
		!! ![Example-1](../media/example-3.svg)
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
		
		call plcont(zl,edge,x,y)
		call resetPen
	end subroutine contour

	subroutine contourf(x,y,z,N)
		!! Plot filled contours.  
		!! ![Example-1](../media/example-3.svg)
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
		integer::Nl,k
		
		xl = x
		yl = y
		zl = z
		Nl = 20
		if(present(N)) Nl = N
		
		edge = [( real(k-1,plflt)/real(Nl-1,plflt)*(maxval(zl)-minval(zl))+minval(zl) ,k=1,Nl)]
		
		fill_width = -1.0_plflt
		cont_width = -1.0_plflt
		cont_color = -1
		
		call plshades(zl,defined,minval(xl),maxval(xl),minval(yl),maxval(yl), &
			& edge,fill_width,cont_color,cont_width)
		call resetPen
	end subroutine contourf

	subroutine quiver(x,y,u,v,s,c,scaling,lineColor,lineStyle,lineWidth)
		!! Plot vectors.  
		!! ![Example-1](../media/example-5.svg)
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
		
		d = [x(2)-x(1),y(2)-y(1)]
		
		xb = mixval(x)
		yb = mixval(y)
		if(present(s)) then
			sl = s
			sl = sl/maxval(sl)
		else
			sl = u**2+v**2
			sl = sqrt(sl)
			sl = sl/maxval(sl)
		end if
		sb = [minval(sl),maxval(sl)]
		if(present(c)) cb = [minval(c),maxval(c)]
		
		scalingl = 1.0_plflt
		if(present(scaling)) scalingl = scaling
		
		if(present(lineColor)) call setColor(lineColor)
		if(present(lineStyle)) call setLineStyle(lineStyle)
		if(present(lineWidth)) call setLineWidth(lineWidth)
		
		do j=1,size(u,2)
			do i=1,size(u,1)
				mag = norm2([ul(i,j),vl(i,j)])
				scl = scalingl*norm2(d)*sl(i,j)
				if(present(c)) call plcol1( (c(i,j)-cb(1))/(cb(2)-cb(1)) )
				call plvect(ul(i:i,j:j)/mag,vl(i:i,j:j)/mag,scl,xl(i:i),yl(j:j))
			end do
		end do
		
		call resetPen
	end subroutine quiver

	subroutine bar(x,y,c,relWidth,fillColor,fillPattern,lineColor,lineWidth)
		!! Create a bar graph.  
		!! ![Example-1](../media/example-6.svg)
		real(wp),dimension(:),intent(in)::x
			!! x-positions of the bars' centers
		real(wp),dimension(:),intent(in)::y
			!! y-positions of the bars' tops
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
		real(plflt)::dx,dxs
		integer::k
		
		if(present(c)) cb = mixval(c)
		dxs = 0.8_wp
		if(present(relWidth)) dxs = relWidth
		if(size(x)>1) then
			dx = dxs*(x(2)-x(1))/2.0_wp
		else
			dx = dxs
		end if
		
		if(present(lineWidth)) call setLineWidth(lineWidth)
		
		do k=1,size(x)
			xl = [x(k)-dx,x(k)-dx,x(k)+dx,x(k)+dx]
			yl = [0.0_wp,y(k),y(k),0.0_wp]
			
			if(present(fillColor)) call setColor(fillColor)
			if(present(fillPattern)) call setFillPattern(fillPattern)
			if(present(c)) call plcol1( (c(k)-cb(1))/(cb(2)-cb(1)) )
			call plfill(xl,yl)
			
			if(present(lineColor)) call setColor(lineColor)
			call plline(xl,yl)
		end do
		call resetPen
	end subroutine bar

	subroutine barh(y,x,c,relWidth,fillColor,fillPattern,lineColor,lineWidth)
		!! Create a bar graph.  
		!! ![Example-1](../media/example-6.svg)
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
		
		if(present(c)) cb = mixval(c)
		dys = 0.8_wp
		if(present(relWidth)) dys = relWidth
		dy = dys*(y(2)-y(1))/2.0_wp
		
		if(present(lineWidth)) call setLineWidth(lineWidth)
		
		do k=1,size(x)
			yl = [y(k)-dy,y(k)-dy,y(k)+dy,y(k)+dy]
			xl = [0.0_wp,x(k),x(k),0.0_wp]
			
			if(present(fillColor)) call setColor(fillColor)
			if(present(fillPattern)) call setFillPattern(fillPattern)
			if(present(c)) call plcol1( (c(k)-cb(1))/(cb(2)-cb(1)) )
			call plfill(xl,yl)
			
			if(present(lineColor)) call setColor(lineColor)
			call plline(xl,yl)
		end do
		call resetPen
	end subroutine barh

	subroutine fillBetween(x,y1,y0,fillColor,fillPattern,lineWidth)
		!! Fill space between two lines
		!! ![Example-1](../media/example-7.svg)
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

	! fill_betweenx
	! hist
	! hist2d
	! hexbin
	! streamplot
	
	! triplot
	! tricolor
	! tricontour
	! tricontourf
	
	! xyzlim
	! surface
	! plot3d

	!========================!
	!= Drawing Pen Routines =!
	!========================!

	subroutine resetPen
		!! Reset pen to default state
		
		call setColor('')
		call setLineStyle('')
		call setLineWidth(0.5_wp)
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
		
		call plcol0(getColorCode(color))
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

	subroutine setup(device,fileName,fontScaling,whiteOnBlack,colormap)
		!! Setup PlPlot library, optionally overriding defaults
		character(*),intent(in),optional::device
			!! Output device to use
		character(*),intent(in),optional::fileName
			!! Name of file(s) to write to
			!! %n will be replaced with the figure number
		real(wp),intent(in),optional::fontScaling
			!! Font scaling relative to default value
		logical,intent(in),optional::whiteOnBlack
		character(*),intent(in),optional::colormap
			!! Colormap to use
		
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
		if(present(fontScaling)) fontScale = fontScaling
		
		!! FIXME
		call plsetopt('geometry','800x600')
		
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
		rgb(getColorCode('y')+1,:) = [255,255,255] ! Yellow
		
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

end module plplot_mod
