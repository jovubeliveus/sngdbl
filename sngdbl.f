c*****************************************************************
c
c NAME:
c	sngdbl
c FUNCTION:
c	benchmark for single and double precision fortran performance
c SYNTAX:
c	sngdbl [len]
c ON INPUT:
c	len: vector lenght
c ON OUTPUT:
c	some timing and Mflop/s results
c CALLS:
c
c COMMENTS:
c
c LIBRARIES:
c
c REFERENCES:
c
c VERSION(S):
c	1. original version                         j.behrens 09/1995
c   2. updated for Scientific Software Development course
c      buggy code...                            j.behrens 06/2012
c
c*****************************************************************
	program sngdbl
c
c parameters
	parameter (maxm=   1536) ! max. matrix size
	parameter (minm=     50) ! max. matrix size
	parameter (maxv=2300000) ! max. vector length
	parameter (maxa=   maxm) ! max. auxiliary vector length
	parameter (maxs=     50) ! max. sparse matrix bandwidth
	parameter (maxr=     20) ! max. no. of benchmarks
c
c local declarations
c fields and matrices
	real smata(maxm,maxm), smatb(maxm,maxm), smatc(maxm,maxm),
     *	  smats(maxs,maxv)
	double precision dmata(maxm,maxm), dmatb(maxm,maxm),
     *	  dmatc(maxm,maxm), dmats(maxs,maxv)
	real sveca(maxv), svecb(maxv)
	double precision dveca(maxv), dvecb(maxv)
	real sscla
	double precision dscla
	integer imati(maxs,maxv)
c
c other declarations
c	character*8 nstri
c this is for cray compilers (posix standard command line input)
	character*8 nstri
	integer i1, i2
c end cray section
	real rmega, flops(maxr), times(maxr), t1, t2, to
	real opsmm, opsmv, opssx, opsdp
	character*8 crout(maxr)
c
c read  commandline arguments
	call getarg(1,nstri)
c this is for cray compilers (posix standard command line input)
c	call pxfgetarg(1,nstri,i1,i2)
c end cray section
	if(nstri.eq.'        ') then
	  nbas= minm
	else if (nstri.eq.'-h      ') then
	  write(6,2000)
	  stop 0
	else
	  read(nstri,*) nbas
	  if(nbas.gt.maxm) then
	    write(6,*) 'ERROR: n > maximum lenght = ', maxm
	    stop 1
	  else if (nbas.lt.minm) then
	    write(6,*) 'ERROR: n < minimum lenght = ', minm
	    stop 1
	  endif
	endif
c
c actual matrix size and vector length
	nmat= nbas
	nvec= nbas* nbas
c
c initialize
	call sinit(smata,smatb,smatc,nmat,nmat,sveca,svecb,nvec)
	call dinit(dmata,dmatb,dmatc,nmat,nmat,dveca,dvecb,nvec)
	sscla= sveca(10)
	dscla= dveca(10)
	do 20 j=1,nvec
	  do 21 i=1,maxs
	    smats(i,j)= float(i)* sveca(i)
	    dmats(i,j)= dble(i)* dveca(i)
	    imati(i,j)= mod(j+i,nvec)
 21	  continue
 20	continue
c
c determine timing routine overhead
	to= 0.
	do 10 i=1,10
	  t1= ticktock()
	  t2= ticktock()
	  to= to+ (t2- t1)
 10	continue
	to= to/ 10.
c
c operation counts
	ir= 0
	opsmm= float(nmat* nmat* nmat* 2)
	opsmv= float(nvec* maxs* 2)
	opssx= float(nvec* 100* 2)
	opsdp= float(nvec* 100* 2)
	rmega= 1./1.e6
c
c --------------- single precision part ------------------
c
c matrix-matrix multiply
	t1= ticktock()
	call smatmat(smata,smatb,smatc,nmat,nmat)
	t2= ticktock()
c
	ir= ir+ 1
	times(ir)= t2- t1- to
	flops(ir)= (opsmm* rmega)/ times(ir)
	crout(ir)='s-matmat'
c
c matrix-matrix multiply (unrolled)
	t1= ticktock()
	call smatmtu(smata,smatb,smatc,nmat,nmat)
	t2= ticktock()
c
	ir= ir+ 1
	times(ir)= t2- t1- to
	flops(ir)= (opsmm* rmega)/ times(ir)
	crout(ir)='s-unroll'
c
c matrix-vector multiply
	t1= ticktock()
	call smatvec(smats,imati,maxs,nvec,sveca,svecb,nvec)
	t2= ticktock()
c
	ir= ir+ 1
	times(ir)= t2- t1- to
	flops(ir)= (opsmv* rmega)/ times(ir)
	crout(ir)='s-matvec'
c
c vector-vector multiply (axpy)
	t1= ticktock()
	call svecsx(sveca,svecb,sscla,nvec)
	t2= ticktock()
c
	ir= ir+ 1
	times(ir)= t2- t1- to
	flops(ir)= (opssx* rmega)/ times(ir)
	crout(ir)='  s-axpy'
c
c vector-vector multiply (dot)
	t1= ticktock()
	call svecdp(sveca,svecb,sscla,nvec)
	t2= ticktock()
c
	ir= ir+ 1
	times(ir)= t2- t1- to
	flops(ir)= (opsdp* rmega)/ times(ir)
	crout(ir)='s-dotpro'
c
c --------------- double precision part ------------------
c
c matrix-matrix multiply
	t1= ticktock()
	call dmatmat(dmata,dmatb,dmatc,nmat,nmat)
	t2= ticktock()
c
	ir= ir+ 1
	times(ir)= t2- t1- to
	flops(ir)= (opsmm* rmega)/ times(ir)
	crout(ir)='d-matmat'
c
c matrix-matrix multiply (unrolled)
	t1= ticktock()
	call dmatmtu(dmata,dmatb,dmatc,nmat,nmat)
	t2= ticktock()
c
	ir= ir+ 1
	times(ir)= t2- t1- to
	flops(ir)= (opsmm* rmega)/ times(ir)
	crout(ir)='d-unroll'
c
c matrix-vector multiply
	t1= ticktock()
	call dmatvec(dmats,imati,maxs,nvec,dveca,dvecb,nvec)
	t2= ticktock()
c
	ir= ir+ 1
	times(ir)= t2- t1- to
	flops(ir)= (opsmv* rmega)/ times(ir)
	crout(ir)='d-matvec'
c
c vector-vector multiply (axpy)
	t1= ticktock()
	call dvecsx(dveca,dvecb,dscla,nvec)
	t2= ticktock()
c
	ir= ir+ 1
	times(ir)= t2- t1- to
	flops(ir)= (opssx* rmega)/ times(ir)
	crout(ir)='  d-axpy'
c
c vector-vector multiply (dot)
	t1= ticktock()
	call dvecdp(dveca,dvecb,dscla,nvec)
	t2= ticktock()
c
	ir= ir+ 1
	times(ir)= t2- t1- to
	flops(ir)= (opsdp* rmega)/ times(ir)
	crout(ir)='d-dotpro'
c
c print out in nice format
	call print_results(times,flops,crout,ir,nmat,nvec)
c
	stop
c
c formats
 2000	format(1x,'USAGE: sngdbl [-h|n]',/
     *	5x,'-h: show this help screen',/
     *	5x,' n: matrix size/vector length',/
     *	1x,'PURPOSE: benchmark for cg-like applications,',/
     *	5x,'single and double precision performance.',/
     *	1x,'(c) j. behrens, 9/95')
	end
c*****************************************************************
      subroutine smatgen(aa,lda,n,norma)
      real aa(lda,n),norma
c
      init = 1325
      norma = 0.0
      do 30 j = 1,n
        do 20 i = 1,lda
          init = mod(3125*init,65536)
          aa(i,j) = (init - 32768.0)/16384.0
          norma = max(abs(aa(i,j)), norma)
   20   continue
   30 continue
      return
      end
c*****************************************************************
      subroutine svecgen(aa,n,norma)
      real aa(n),norma
c
      init = 1325
      norma = 0.0
      do 30 j = 1,n
        init = mod(3125*init,65536)
        aa(j) = (init - 32768.0)/16384.0
        norma = max(abs(aa(j)), norma)
   30 continue
      return
      end
c*****************************************************************
	subroutine sinit(aa,bb,ee,lda,nm,cc,dd,nv)
c
c local declarations
	real aa(lda,nm), bb(lda,nm), ee(lda,nm), cc(nv), dd(nv), rn
	integer lda, nm, nv
c
c call matgen for aa and bb
	call smatgen(aa,lda,nm,rn)
	call smatgen(bb,lda,nm,rn)
	call smatgen(ee,lda,nm,rn)
c
	call svecgen(cc,nv,rn)
	call svecgen(dd,nv,rn)
c
	return
	end
c*****************************************************************
      subroutine dmatgen(aa,lda,n,norma)
      double precision  aa(lda,n),norma
c
      init = 1325
      norma = 0.0
      do 30 j = 1,n
        do 20 i = 1,lda
          init = mod(3125*init,65536)
          aa(i,j) = (init - 32768.0)/16384.0
          norma = max(abs(aa(i,j)), norma)
   20   continue
   30 continue
      return
      end
c*****************************************************************
      subroutine dvecgen(aa,n,norma)
      double precision  aa(n),norma
c
      init = 1325
      norma = 0.0
      do 30 j = 1,n
        init = mod(3125*init,65536)
        aa(j) = (init - 32768.0)/16384.0
        norma = max(abs(aa(j)), norma)
   30 continue
      return
      end
c*****************************************************************
	subroutine dinit(aa,bb,ee,lda,nm,cc,dd,nv)
c
c local declarations
	double precision aa(lda,nm), bb(lda,nm), ee(lda,nm),
     *	cc(nv), dd(nv), rn
	integer lda, nm, nv
c
c call matgen for aa and bb
	call dmatgen(aa,lda,nm,rn)
	call dmatgen(bb,lda,nm,rn)
	call dmatgen(ee,lda,nm,rn)
c
	call dvecgen(cc,nv,rn)
	call dvecgen(dd,nv,rn)
c
	return
	end
c*****************************************************************
c*****************************************************************
	subroutine smatmat(aa,bb,cc,lda,n)
c
c local declarations
	real aa(lda,n), bb(lda,n), cc(lda,n)
c
c matrixmultiplication
	do 10 k=1,n
	  do 10 j=1,n
	    do 10 i=1,n
 10	      cc(i,j)= cc(i,j)+ aa(i,k)* bb(k,j)
c
	return
	end
c*****************************************************************
	subroutine smatmtu(aa,bb,cc,lda,n)
c
c local declarations
	parameter (lunr=4)
	real aa(lda,n), bb(lda,n), cc(lda,n)
	integer n1, n2, n3
c
c calculate some loop bounds
	n1= int(n/lunr)
	n3= n1* lunr
	n2= n- n3
c
c matrixmultiplication (with loop unrolling)
	do 10 k=1,n3,lunr
	  do 10 j=1,n
	    do 10 i=1,n
 10	      cc(i,j)= cc(i,j)+ aa(i,k)* bb(k,j)
     *			      + aa(i,k+1)* bb(k+1,j)
     *			      + aa(i,k+2)* bb(k+2,j)
     *			      + aa(i,k+3)* bb(k+3,j)
c     *			      + aa(i,k+4)* bb(k+4,j)
c     *			      + aa(i,k+5)* bb(k+5,j)
c     *			      + aa(i,k+6)* bb(k+6,j)
c     *			      + aa(i,k+7)* bb(k+7,j)
c
	do 20 k=n3+1,n
	  do 20 j=1,n
	    do 20 i=1,n
 20	      cc(i,j)= cc(i,j)+ aa(i,k)* bb(k,j)
c
	return
	end
c*****************************************************************
	subroutine smatvec(aa,ii,lda,nm,bb,cc,nv)
c
c local declarations
	real aa(lda,nm), bb(nv), cc(nv)
	integer ii(lda,nm)
c
c matrix-vector multiplication
	do 10 j=1,nm
	  do 10 i=1,lda
 10	      cc(j)= cc(j)+ aa(i,j)* bb(ii(i,j))
c
	return
	end
c*****************************************************************
	subroutine svecsx(aa,bb,cc,nv)
c
c local declarations
	real aa(nv), bb(nv), cc
c
c axpy multiplication
	do 15 i=1,100 ! dummy loop precise timing
	aa(i)= bb(i+1)
	do 10 j=1,nv
 10	  bb(j)= aa(j)* cc+ bb(j)
 15	continue
c
	return
	end
c*****************************************************************
	subroutine svecdp(aa,bb,cc,nv)
c
c local declarations
	real aa(nv), bb(nv), cc
c
c dotproduct multiplication
	do 15 i=1,100 ! dummy loop precise timing
	aa(i)= bb(i+1)
	cc= 0.0
	do 10 j=1,nv
 10	  cc= cc+ aa(j)* bb(j)
 15	continue
c
	return
	end
c*****************************************************************
c*****************************************************************
	subroutine dmatmat(aa,bb,cc,lda,n)
c
c local declarations
	double precision aa(lda,n), bb(lda,n), cc(lda,n)
c
c matrixmultiplication
	do 10 k=1,n
	  do 10 j=1,n
	    do 10 i=1,n
 10	      cc(i,j)= cc(i,j)+ aa(i,k)* bb(k,j)
c
	return
	end
c*****************************************************************
	subroutine dmatmtu(aa,bb,cc,lda,n)
c
c local declarations
	parameter (lunr=4)
	double precision aa(lda,n), bb(lda,n), cc(lda,n)
	integer n1, n2, n3
c
c calculate some loop bounds
	n1= int(n/lunr)
	n3= n1* lunr
	n2= n- n3
c
c matrixmultiplication (with loop unrolling)
	do 10 k=1,n3,lunr
	  do 10 j=1,n
	    do 10 i=1,n
 10	      cc(i,j)= cc(i,j)+ aa(i,k)* bb(k,j)
     *			      + aa(i,k+1)* bb(k+1,j)
     *			      + aa(i,k+2)* bb(k+2,j)
     *			      + aa(i,k+3)* bb(k+3,j)
c     *			      + aa(i,k+4)* bb(k+4,j)
c     *			      + aa(i,k+5)* bb(k+5,j)
c     *			      + aa(i,k+6)* bb(k+6,j)
c     *			      + aa(i,k+7)* bb(k+7,j)
c
	do 20 k=n3+1,n
	  do 20 j=1,n
	    do 20 i=1,n
 20	      cc(i,j)= cc(i,j)+ aa(i,k)* bb(k,j)
c
	return
	end
c*****************************************************************
	subroutine dmatvec(aa,ii,lda,nm,bb,cc,nv)
c
c local declarations
	double precision aa(lda,nm), bb(nv), cc(nv)
	integer ii(lda,nm)
c
c matrix-vector multiplication
	do 10 j=1,nm
	  do 10 i=1,lda
 10	      cc(j)= cc(j)+ aa(i,j)* bb(ii(i,j))
c
	return
	end
c*****************************************************************
	subroutine dvecsx(aa,bb,cc,nv)
c
c local declarations
	double precision aa(nv), bb(nv), cc
c
c axpy multiplication
	do 15 i=1,100 ! dummy loop precise timing
	aa(i)= bb(i+1)
	do 10 j=1,nv
 10	  bb(j)= aa(j)* cc+ bb(j)
 15	continue
c
	return
	end
c*****************************************************************
	subroutine dvecdp(aa,bb,cc,nv)
c
c local declarations
	double precision aa(nv), bb(nv), cc
c
c dotproduct multiplication
	do 15 i=1,100 ! dummy loop precise timing
	aa(i)= bb(i+1)
	cc= 0.0
	do 10 j=1,nv
 10	  cc= cc+ aa(j)* bb(j)
 15	continue
c
	return
	end
c*****************************************************************
	subroutine print_results(times,flops,crout,ir,nm,nv)
c
c local declarations
	integer ir
	character*8 crout(ir)
	real times(ir), flops(ir)
c
c print header
	write(6,1000)
	write(6,1010) nm, nv
	write(6,1000)
c
c print values for each routine
	do 10 i=1,ir
	  write(6,1020) crout(i)
	  write(6,1030) times(i), flops(i)
	  write(6,1001)
 10	continue
c
	write(6,1000)
c
c formats
 1001  FORMAT(1x,
     *    '|------------------------------------------------------|'
     *    )
 1000  FORMAT(1x,
     *    '|======================================================|'
     *    )
 1010  FORMAT(
     *	1x,'|',10x,'Benchmark for CG-like Applications',10x,'|'/
     *  1x,'|',15x,'LinuX Version 1.0 (9/95)',15x,'|'/
     *  1x,'|',18x,'(c) by J. Behrens',19x,'|'/
     *	1x,'|',15x,'Matrix size: ',i8,'**2',15x,'|'/
     *	1x,'|',15x,'Vector length: ',i8,16x,'|')
 1020  FORMAT(
     *	1x,'|',13x,'Results for routine ',a8,13x,'|')
 1030  FORMAT(
     *	1x,'|',6x,'Time (s): ',e11.4,' Mflop/s: ',e11.4,6x,'|')
	return
	end
c*****************************************************************
c this is for compatibility reasons
c*****************************************************************
	real function ticktock()
c
	ticktock= real(tick())
	return
	end
