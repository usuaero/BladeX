module view_m
    use prop_m
    implicit none
    
contains


!-----------------------------------------------------------------------------------------------------------

subroutine view_stl(t)
    type(prop_t) :: t
    type(section_t),pointer :: si
    type(airfoil_t),pointer :: af1
    type(airfoil_t),pointer :: af2
    real,allocatable,dimension(:,:) :: af_points1,af_points2
    character(100) :: filename
    integer :: i,isec,af_datasize,iblade
    integer :: ierror = 0

    do i=1,size(airfoils)
        af1 => airfoils(i);
        call af_create_geom_from_file(af1,DB_Airfoil)
    end do
    af1 => t%af1
    af2 => t%af2

    filename = trim(adjustl(t%master_filename))//'_view.stl'
    open(unit = 10, File = filename, action = 'write', iostat = ierror)

!    filename = trim(adjustl(t%master_filename))//'_view.web'
!    open(unit = 20, File = filename, action = 'write', iostat = ierror)

    write(10,'(A)') 'solid geom'

    af_datasize = af1%geom%datasize
    if(af2%geom%datasize .ne. af_datasize) then
        write(*,*) 'Both root and tip airfoils must have same number of nodes'
        stop
    end if
    allocate(af_points1(af_datasize,3))
    allocate(af_points2(af_datasize,3))

    do iblade=1,t%nblades
        do isec=1,t%nSec
            si => t%sec(isec)
            call view_create_local_airfoil(af1,af2,t%side,si%percent_1,si%chord_1,&
                                        & si%twist1,si%dihedral1,af_datasize,si%P1(:),af_points1)
            call view_create_local_airfoil(af1,af2,t%side,si%percent_2,si%chord_2,&
                                        & si%twist2,si%dihedral2,af_datasize,si%P2(:),af_points2)

            call view_rotate_z(real(iblade-1)*2.0*pi/real(t%nblades),af_datasize,af_points1)
            call view_rotate_z(real(iblade-1)*2.0*pi/real(t%nblades),af_datasize,af_points2)

            call view_create_stl_shell(af_datasize,af_points1,af_points2)
        end do

        if(t%side == 'right') then
            si => t%sec(1)
            call view_create_local_airfoil(af1,af2,t%side,si%percent_1,si%chord_1,&
            & si%twist1,si%dihedral1,af_datasize,si%P1(:),af_points1)
            si => t%sec(t%nSec)
            call view_create_local_airfoil(af1,af2,t%side,si%percent_2,si%chord_2,&
            & si%twist2,si%dihedral2,af_datasize,si%P2(:),af_points2)
        else
            si => t%sec(1)
            call view_create_local_airfoil(af1,af2,t%side,si%percent_2,si%chord_2,&
                                        & si%twist2,si%dihedral2,af_datasize,si%P2(:),af_points2)
            si => t%sec(t%nSec)
            call view_create_local_airfoil(af1,af2,t%side,si%percent_1,si%chord_1,&
                                        & si%twist1,si%dihedral1,af_datasize,si%P1(:),af_points1)
        end if

!        if(t%is_linear.eq.1) call view_create_stl_shell(af_datasize,af_points1,af_points2)
!        call view_create_stl_rib(af_datasize,af_points1)
!        call view_create_stl_rib(af_datasize,af_points2)
    end do

    write(10,*) 'endsolid geom'
    close(10)
!    close(20)
end subroutine view_stl

!-----------------------------------------------------------------------------------------------------------
subroutine view_rotate_z(theta,datasize,af_points)
    integer :: datasize,i
    real :: theta,af_points(datasize,3),point(3),phi,radius

    do i=1,datasize
        point(:) = af_points(i,:)
        phi = atan2(point(2),point(1))
        radius = sqrt(point(1)**2 + point(2)**2)
        af_points(i,1) = radius*cos(theta+phi)
        af_points(i,2) = radius*sin(theta+phi)
    end do

end subroutine view_rotate_z

!-----------------------------------------------------------------------------------------------------------
subroutine view_create_stl_shell(datasize,af_points1,af_points2)
    integer :: datasize,i
    real :: af_points1(datasize,3),af_points2(datasize,3)
    real :: P1(3),P2(3),P3(3)

    !Outer surface
    do i=1,datasize-1
        P1(:) = af_points1(i,:)
        P2(:) = af_points2(i,:)
        P3(:) = af_points1(i+1,:)
        
        call view_add_stl_triangle(P1,P3,P2)

        P1 = P2
        P2(:) = af_points2(i+1,:)
        call view_add_stl_triangle(P1,P3,P2)
    end do
    !Close Surface
    i = datasize
    P1(:) = af_points1(i,:)
    P2(:) = af_points1(1,:)
    P3(:) = af_points2(i,:)

    call view_add_stl_triangle(P1,P3,P2)

    P1 = P2
    P2(:) = af_points2(1,:)
    call view_add_stl_triangle(P1,P3,P2)
end subroutine view_create_stl_shell

!-----------------------------------------------------------------------------------------------------------
subroutine view_create_stl_rib(datasize,af_points)
    integer :: datasize,i
    real :: af_points(datasize,3)
    real :: P1(3),P2(3),P3(3)

    do i=1,(datasize-2)/2
        P1(:) = af_points(i,:)
        P2(:) = af_points(datasize-i+1,:)
        P3(:) = af_points(i+1,:)
        call view_add_stl_triangle(P1,P3,P2)
        
        P1 = P2
        P2(:) = af_points(datasize-i,:)
        call view_add_stl_triangle(P1,P3,P2)
    end do
end subroutine view_create_stl_rib

!-----------------------------------------------------------------------------------------------------------
subroutine view_add_stl_triangle(P1,P2,P3)
    real :: P1(3),P2(3),P3(3),norm(3)
    110 Format(A15, 3ES25.13)
    120 Format(9ES25.13)

    call math_plane_normal(P1,P2,P3,norm)
    write(10,110) 'facet normal ',norm(:)
    write(10,*) 'outer loop'
    write(10,110) 'vertex ',P1(:)
    write(10,110) 'vertex ',P2(:)
    write(10,110) 'vertex ',P3(:)
    write(10,*) 'endloop'
    write(10,*) 'endfacet'
    
!    write(20,120) P1(:),P2(:),P3(:)
end subroutine view_add_stl_triangle

!-----------------------------------------------------------------------------------------------------------
subroutine view_create_local_airfoil(af1,af2,side,percent,chord,twist,dihedral,datasize,point,output)
    type(airfoil_t),pointer :: af1
    type(airfoil_t),pointer :: af2
    character(5) :: side
    real :: percent,chord,twist,dihedral
    integer :: datasize,i
    real :: point(3),output(datasize,3)

    output(:,1:2) = af1%geom%RawData(:,:) + percent*(af2%geom%RawData(:,:) - af1%geom%RawData(:,:))
    output(:,1) = output(:,1) - 0.25
    output = chord*output
    output(:,1) = -output(:,1) !flip x
    output(:,3) = -output(:,2) !assign y to z
    output(:,2) = 0.0 !set y to zero
    
    
    if(side.eq.'left') then
!        twist = -twist
        dihedral = -dihedral
    end if
    
    do i=1,datasize
        call math_rot_y(output(i,:),twist)
        call math_rot_x(output(i,:),-dihedral)
        output(i,:) = output(i,:) + point(:)
    end do
    
end subroutine view_create_local_airfoil

end module view_m
