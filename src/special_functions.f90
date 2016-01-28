module special_functions_m
    use prop_m
    implicit none
    
contains

!-----------------------------------------------------------------------------------------------------------
subroutine sf_thrust_plots(t,json_command) 
    use prop_m
    implicit none
    type(prop_t) :: t
    type(json_value),intent(in),pointer :: json_command
    type(json_value),pointer    :: jp
    integer :: nsteps = 11
    real :: temp,mult
    character(100) :: filename
    integer :: ierror
    
    mult = 0.5

    nullify(jp)
    call json_value_create(jp)

    filename = trim(adjustl(t%master_filename))//'_thrust_plots.txt'
    open(unit = 10, File = filename, action = 'write', iostat = ierror)
    write(10,*) '     Diameter[m]              Thrust[N]                Throttle                 PowerRequired[W]         &
                     &Current[A]               SystemEfficiency         Endurance[min]           PropellerRPM'
    temp = t%diameter
!    call sf_run_sequence(t,'diameter','thrust',(1.0-mult)*t%diameter,(1.0+mult)*t%diameter,nsteps)
    t%diameter = temp

    write(10,*)
    write(10,*) '     Pitch[m]                 Thrust[N]                Throttle                 PowerRequired[W]         &
                     &Current[A]               SystemEfficiency         Endurance[min]           PropellerRPM'
    temp = t%pitch
!    call sf_run_sequence(t,'pitch','thrust',(1.0-mult)*t%pitch,(1.0+mult)*t%pitch,nsteps)
    t%pitch = temp
    
    write(10,*)
    write(10,*) '     Kv[RPM/V]                Thrust[N]                Throttle                 PowerRequired[W]         &
                     &Current[A]               SystemEfficiency         Endurance[min]           PropellerRPM'
    temp = t%motor%kv
!    call sf_run_sequence(t,'kv',(1.0-mult)*t%motor%kv,(1.0+mult)*t%motor%kv,nsteps,'thrust',1.0) !fix this
    t%motor%kv = temp

    close(10)
end subroutine sf_thrust_plots

!-----------------------------------------------------------------------------------------------------------
subroutine sf_sequence(t,json_command) 
    use prop_m
    implicit none
    type(prop_t) :: t
    type(json_value),intent(in),pointer :: json_command
    character(len=:),allocatable :: cval,varname,runtype
    character(100) :: filename
    real :: xval,xstart,xend,results(6),runvalue
    integer :: nsteps,ierror
    
    call json_get(json_command,'variable', varname, json_found); call json_check();
    xstart = json_required_real(json_command,'start')
    xend   = json_required_real(json_command,'end')
    nsteps = json_optional_integer(json_command,'steps',10)
    call json_get(json_command,'holding', runtype, json_found); call json_check();
    runvalue   = json_required_real(json_command,'value')

    !Get filename if specified
    call json_get(json_command,'filename', cval,json_found);
    if(json_failed() .or. (trim(cval).eq.'')) then !No filename specified
        call json_clear_exceptions()
        filename = trim(adjustl(t%master_filename))//'_'//trim(adjustl(varname))//'_sequence.txt'
    else
        filename = trim(cval)
    end if

    open(unit = 10, File = filename, action = 'write', iostat = ierror)
    if(varname.eq.'diameter') then
    write(10,*) '     Diameter[m]              Thrust[N]                Throttle                 PowerRequired[W]         &
                     &Current[A]               SystemEfficiency         Endurance[min]           PropellerRPM'
    end if
    if(varname.eq.'pitch') then
    write(10,*) '     Pitch[m]                 Thrust[N]                Throttle                 PowerRequired[W]         &
                     &Current[A]               SystemEfficiency         Endurance[min]           PropellerRPM'
    end if
    if(varname.eq.'kv') then
    write(10,*) '     Kv[RPM/V]                Thrust[N]                Throttle                 PowerRequired[W]         &
                     &Current[A]               SystemEfficiency         Endurance[min]           PropellerRPM'
    end if

    call sf_run_sequence(t,varname,xstart,xend,nsteps,runtype,runvalue)
    
    close(10)
end subroutine sf_sequence

!-----------------------------------------------------------------------------------------------------------
subroutine sf_run_sequence(t,varname,xstart,xend,nsteps,runtype,runvalue)
    use prop_m
    implicit none
    type(prop_t) :: t
!    type(json_value),pointer,intent(in):: jp
    character(len=*),intent(in) :: varname,runtype
    real :: xval,xstart,xend,xinc,runvalue
    integer :: i,nsteps
    real,allocatable,dimension(:,:) :: results
    real :: temp_diameter,temp_pitch,temp_kv
    120 Format(8ES25.13)

    !Store current settings
    temp_diameter = t%diameter
    temp_pitch    = t%pitch
    temp_kv       = t%motor%kv

    allocate(results(8,nsteps))
    xinc = (xend-xstart)/real(nsteps-1)
    xval = xstart
    do i=1,nsteps
        results(1,i) = xval
        call sf_set_variable(t,varname,xval)
        call sf_run_single(t,runtype,runvalue,results(2:8,i))
        xval = xval + xinc
    end do
    
    do i=1,nsteps
        write(10,120) results(:,i)
    end do
    
    !restore previous settings
    t%diameter = temp_diameter
    t%pitch    = temp_pitch
    t%motor%kv = temp_kv

end subroutine sf_run_sequence

!-----------------------------------------------------------------------------------------------------------
subroutine sf_set_variable(t,varname,value)
    use prop_m
    implicit none
    type(prop_t) :: t
    character(len=*),intent(in) :: varname
    real :: value
    
    if(varname.eq.'diameter') t%diameter = value
    if(varname.eq.'pitch')    t%pitch = value
    if(varname.eq.'kv')       t%motor%kv = value
end subroutine sf_set_variable
                
!-----------------------------------------------------------------------------------------------------------
subroutine sf_run_single(t,runtype,runvalue,results)
    use prop_m
    implicit none
    type(prop_t) :: t
    character(len=*),intent(in) :: runtype
    real :: results(7),runvalue

    call prop_setup(t)
    select case (runtype)
        case ('rpm')
            call prop_run_rpm(t,runvalue)
        case ('throttle')
            call prop_run_throttle(t,runvalue)
        case ('thrust')
            call prop_run_thrust(t,runvalue)
    end select
    results(:) = 0.0

    results(1) = t%Thrust
    results(2) = t%throttle
    results(3) = t%battery%power
    results(4) = t%battery%current
    results(5) = t%eta
    results(6) = t%battery%endurance
    results(7) = t%rpm
end subroutine sf_run_single

end module special_functions_m