!**************************************************************************************************
! Copyright (C), 2015, NuTheL Lab, Xi��an Jiaotong University, P.R. China
! File name: INPUT.f90
! Author:Li Xinyu        
! Version:1.0        
! Date:2020/9/29
! Description: This is the module of reading equations from .txt and transfer them to solvers
! Others:  
! Function List:  
! History:     
!    1. Date: 2020/9/29
!        Author: Li Xinyu
!        Modification: Began developing the program
!    2.	Date: 2020/9/30
!        Author: Li Xinyu
!        Modification: Change the rule of reading to read new cards.
!**************************************************************************************************
MODULE INPUT
	IMPLICIT NONE
	INTEGER,PARAMETER :: fileID = 10
	CONTAINS
	
	subroutine readtxt(A,b,x,N,methods)
	!*************************************************
	! Version:1.0     
	! Date: 2020/9/30
	! Coded by:	Li Xinyu 
	! Calls: 
	! Called By: SOLVERSFORLINEAREQUATIONS(main program)
	! Input: A,b,x,N,methods     
	! Output: A,b,x,N,methods
	! Return:  
	! Others:       
	!*************************************************
		integer i,j,N,methods !��ȡ��ѭ������
		!real(kind = 8) Na(1)
		real(kind = 8),allocatable :: A(:,:), b(:), x(:) !�������A������b�ʹ��������x
		character test !�������ַ�
		open (unit = fileID,file = 'fin.txt')
		
		read (fileID,*) test !��ȡ#�����ж�
		if (test == '#') then
			read (fileID,'(1I3)') N
			write (*,*) '�����С�������'
			write (*,*) N
		else
			write (*,*) '�����ʽ����'
		end if
		
		allocate(A(N,N)) !����ռ�
		allocate(b(N))
		allocate(x(N))
		
		write (*,*) !��һ��
		read (fileID,*) test!������һ��
		if (test == '#') then
			read (fileID,*) ((A(i,j),j=1,N),i=1,N) !��ȡA��������A������д�����ʰ����ȶ�j�ٶ�i��˳��(�ȿ����в��䣬����)�����ڴ��Ų���ʽ�෴�����Ϊ�˼ӿ��ٶȣ�д�����ʱ���Ҫת��д��
			write (*,*) "ϵ������"
			write (*,*) A
		else
			write (*,*) '�����ʽ����'
		end if
		
		write (*,*) !��һ��
		read (fileID,*) test!������һ��
		if (test == '#') then
			read (fileID,*) b !��ȡbת��β������
			write (*,*) "β������"
			do i=1,N
				write (*,'(D15.3)') b(i)
			end do
		else
			write (*,*) '�����ʽ����'
		end if
		
		write (*,*) !��һ��
		read (fileID,*) test!������һ��
		if (test == '#') then
			read (fileID,*) methods !��ȡ�ⷨ
			write (*,*) "�ⷨ��"
			if (methods == 1) then !ѡ��ⷨ��1=����Ԫ��˹
				write (*,*) "����Ԫ��˹"
			else if (methods == 2) then !ѡ��ⷨ��2=LU
				write (*,*) "LU"
			end if
			write (*,*) !��һ��
		else
			write (*,*) '�����ʽ����'
		end if
		
		close (fileID) !��ʱ�ر�fin.txt
		!return
		!call Gausssolve(A,b,x,N)  !���ûش�����
	end subroutine readtxt
END MODULE INPUT	