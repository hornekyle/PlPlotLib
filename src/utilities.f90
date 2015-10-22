module utilities_mod
	!! Utility module containing miscellaneous tools that don't
	!! quite fit anywhere else.
	use iso_fortran_env
	use kinds_mod
	implicit none
	private
	
	integer,parameter::stdin  = INPUT_UNIT
	integer,parameter::stdout = OUTPUT_UNIT
	
	interface mixval
		!! Return a 2-vector comprising the minimum and maximum values of an array
		module procedure mixval_1
		module procedure mixval_2
		module procedure mixval_3
	end interface
	
	interface span
		!! Return a the maximum-minumum values of an array
		module procedure span_1
		module procedure span_2
		module procedure span_3
	end interface
	
	interface flatten
		!! Reduce an array to one dimension
		module procedure flatten_2
		module procedure flatten_3
	end interface
	
	public::stdin
	public::stdout
	
	public::mixval
	public::span
	public::linspace
	
	public::startsWith
	public::endsWith
	
	public::meshGridX
	public::meshGridY
	
	public::randomNormal
	public::randomUniform
	public::mean
	public::stdev
	
	public::flatten
	
	public::colorize
	public::int2char
	public::real2char
	
	public::showProgress
	
contains

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

	function span_1(x) result(o)
		!! Return hi-low for an array
		real(wp),dimension(:),intent(in)::x
			!! Array to find span in
		real(wp)::o
		
		o = maxval(x)-minval(x)
	end function span_1

	function span_2(x) result(o)
		!! Return hi-low for an array
		real(wp),dimension(:,:),intent(in)::x
			!! Array to find span in
		real(wp)::o
		
		o = maxval(x)-minval(x)
	end function span_2

	function span_3(x) result(o)
		!! Return hi-low for an array
		real(wp),dimension(:,:,:),intent(in)::x
			!! Array to find span in
		real(wp)::o
		
		o = maxval(x)-minval(x)
	end function span_3

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

	function randomNormal() result(o)
		!! Return a sample from an approximate normal distribution
		!! with a mean of \(\mu=0\) and a standard deviation of
		!! \(\sigma=1\). In this approximate distribution, \(x\in[-6,6]\).
		real(wp)::o
		real(wp),dimension(12)::x

		call random_number(x)
		o = sum(x)-6.0_wp
	end function randomNormal

	function randomUniform() result(o)
		!! Return a sample from a uniform distribution
		!! in the range \(x\in[-1,1]\).
		real(wp)::o

		call random_number(o)
		o = o*2.0_wp-1.0_wp
	end function randomUniform

	function flatten_2(A) result(o)
		!! Convert a 2d array to 1d
		real(wp),dimension(:,:),intent(in)::A
			!! Array to convert
		real(wp),dimension(:),allocatable::o
		
		o = reshape(A,[size(A)])
	end function flatten_2

	function flatten_3(A) result(o)
		!! Convert a 3d array to 1d
		real(wp),dimension(:,:,:),intent(in)::A
			!! Array to convert
		real(wp),dimension(:),allocatable::o
		
		o = reshape(A,[size(A)])
	end function flatten_3

	function meshGridX(x,y) result(o)
		!! Construct a 2d array of X values from a structured grid
		real(wp),dimension(:),intent(in)::x
			!! x-positions in grid
		real(wp),dimension(:),intent(in)::y
			!! y-positions in grid
		real(wp),dimension(:,:),allocatable::o
		
		integer::Nx,Ny
		integer::i,j
		
		Nx = size(x)
		Ny = size(y)
		
		allocate(o(Nx,Ny))
		
		forall(i=1:Nx,j=1:Ny) o(i,j) = x(i)
	end function meshGridX

	function meshGridY(x,y) result(o)
		!! Construct a 2d array of Y values from a structured grid
		real(wp),dimension(:),intent(in)::x
			!! x-positions in grid
		real(wp),dimension(:),intent(in)::y
			!! y-positions in grid
		real(wp),dimension(:,:),allocatable::o
		
		integer::Nx,Ny
		integer::i,j
		
		Nx = size(x)
		Ny = size(y)
		
		allocate(o(Nx,Ny))
		
		forall(i=1:Nx,j=1:Ny) o(i,j) = y(j)
	end function meshGridY

	function colorize(s,c) result(o)
		!! Add terminal format codes to coloize a string
		character(*),intent(in)::s
			!! String to colorize
		integer,dimension(3)::c ! c in [0,5]
			!! Color code in [r,g,b] where \(r,g,b\in[0,5]\)
		character(:),allocatable::o
		
		character(1),parameter::CR  = achar(13)
		character(1),parameter::ESC = achar(27)
		
		character(20)::pre
		character(3)::cb
		
		write(cb,'(1I3)') 36*c(1)+6*c(2)+c(3)+16
		pre = ESC//'[38;5;'//trim(adjustl(cb))//'m'
		o = trim(pre)//s//ESC//'[0m'
	end function colorize

	elemental function real2char(a,f,l) result(o)
		!! Convert a real to a character
		real(wp),intent(in)::a
			!! Real value to convert
		character(*),optional,intent(in)::f
			!! Format of result
		integer,optional,intent(in)::l
			!! Length of result
		character(:),allocatable::o
		
		character(128)::buf
		
		if(present(l)) then
			allocate(character(l)::o)
			if(present(f)) then
				write(o,'('//f//')') a
			else
				write(o,*) a
			end if
		else
			if(present(f)) then
				write(buf,'('//f//')') a
			else
				write(buf,*) a
			end if
			o = trim(adjustl(buf))
		end if
	end function real2char

	elemental function int2char(a,f,l) result(o)
		!! Convert an integer to a character
		integer,intent(in)::a
			!! Integer value to convert
		character(*),optional,intent(in)::f
			!! Format of result
		integer,optional,intent(in)::l
			!! Length of result
		character(:),allocatable::o
		
		character(128)::buf
		
		if(present(l)) then
			allocate(character(l)::o)
			if(present(f)) then
				write(o,'('//f//')') a
			else
				write(o,*) a
			end if
		else
			if(present(f)) then
				write(buf,'('//f//')') a
			else
				write(buf,*) a
			end if
			o = trim(adjustl(buf))
		end if
	end function int2char

	subroutine showProgress(m,p)
		!! Show a progress bar with a message
		character(*),intent(in)::m
			!! Message to show
		real(wp),intent(in)::p
			!! Progress level \(p\in[0,1]\)
		
		real(wp)::r
		real(wp),save::po
		integer::N,k
		
		N = 40
		
		if(p<=0.0_wp) then
			po = p
		end if
		if(p-po<0.05 .and. p<1.0_wp) then
			return
		else
			po = p
		end if
		
		write(stdout,'(1A)',advance='no') achar(13)//colorize(m//' [',[5,5,0])
		do k=1,N
			r = real(k-1,wp)/real(N-1,wp)
			if(r<=p) then
				write(stdout,'(1A)',advance='no') colorize('=',cmap(r,[0.0_wp,1.0_wp]))
			else
				write(stdout,'(1A)',advance='no') colorize(' ',[0,0,0])
			end if
		end do
		write(stdout,'(1A,1A,1X,1A)',advance='no') colorize('] ',[5,5,0]), &
		& colorize(real2char(100.0_wp*p,'1F5.1'),cmap(p,[0.0_wp,1.0_wp])), &
		& colorize('%',[5,5,0])
		if(p>=1.0_wp) write(stdout,'(1A)') ''
		flush(stdout)
	end subroutine showProgress

	function cmap(v,r) result(c)
		!! Sample a color from a cool-warm colormap for colorize
		real(wp),intent(in)::v
			!! Value to sample
		real(wp),dimension(2),intent(in)::r
			!! Range to sample from
		integer,dimension(3)::c
		
		integer::s
		
		if(v<sum(r)/2.0_wp) then
			s = nint((v-r(1))/(sum(r)/2.0_wp-r(1))*5.0_wp)
			c = [s,s,5]
		else
			s = 5-nint((v-sum(r)/2.0_wp)/(r(2)-sum(r)/2.0_wp)*5.0_wp)
			c = [5,s,s]
		end if
	end function cmap

	function mean(d) result(o)
		!! Compute the arithmetic mean of an array
		real(wp),dimension(:),intent(in)::d
		real(wp)::o
		
		o = sum(d)/real(size(d),wp)
	end function mean

	function stdev(d) result(o)
		!! Compute the standard deviation of an array
		real(wp),dimension(:),intent(in)::d
		real(wp)::o
		
		o = sqrt(sum((d-mean(d))**2)/real(size(d)-1,wp))
	end function stdev

end module utilities_mod
