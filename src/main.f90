!gfortran -fdefault-real-8 json.f90 math.f90 dataset.f90 airfoil.f90 section.f90 prop.f90 view.f90 main.f90
program main
    use view_m
    use special_functions_m
    use prop_m
    implicit none
    type(prop_t) :: myprop
    character(100) :: filename
    type(json_value),pointer :: json_run, json_command
    integer :: error = 0
    integer :: i,nrun_types,dorun

    real :: time1,time2
    
    call cpu_time(time1)
    write(*,*) '-----------------------------------------------'
    write(*,*) '|                                             |'
    write(*,*) '|                         BBBBB               |'
    write(*,*) '|                       BB   BB               |'
    write(*,*) '|                     BB     BB               |'
    write(*,*) '|                    BB      BB               |'
    write(*,*) '|                  BB        BB               |'
    write(*,*) '|                 BB         BB               |'
    write(*,*) '|               BB           BB               |'
    write(*,*) '|              BB        BBBBBB               |'
    write(*,*) '|                                             |'
    write(*,*) '|                 BladeX 1.0                  |'
    write(*,*) '|                                             |'
    write(*,*) '|           (c) USU Aero Lab 2016            |'
    write(*,*) '|                                             |'
    write(*,*) '|          This software comes with           |'
    write(*,*) '| ABSOLUTELY NO WARRANTY EXPRESSED OR IMPLIED |'
    write(*,*) '|                                             |'
    write(*,*) '|           Submit bug reports to:            |'
    write(*,*) '|           doug.hunsaker@usu.edu             |'
    write(*,*) '-----------------------------------------------'
    write(*,*)

    call get_command_argument(1,filename)
    myprop%master_filename = filename
    
    call prop_set_defaults(myprop)
    call prop_load_json(myprop,error)
    
    if(error.eq.1) STOP
    call prop_init_setup(myprop)

    call myprop%json%get('run', json_run)
    nrun_types = json_value_count(json_run)
!    write(*,*) 'Number of commands to run : ',nrun_types

    do i=1,nrun_types
        call json_value_get(json_run,i,json_command)
        run_type = trim(json_command%name)
        dorun = 1
        call json_get(json_command,'run',dorun,json_found)
        if(json_failed()) dorun = 1; !automatically run command if .run sub-command doesn't exist
        call json_clear_exceptions()

        if(dorun .eq. 1) then
            write(*,*) 
            write(*,*) 'Running command : ',run_type

            select case (run_type)
                case ('stl')
                    call view_stl(myprop)
                case ('rpm')
                    call prop_run_rpm(myprop,json_required_real(json_command,'value'))
                case ('throttle')
                    call prop_run_throttle(myprop,json_required_real(json_command,'value'))
                case ('thrust')
                    call prop_run_thrust(myprop,json_required_real(json_command,'value'))
                case ('sequence')
                    call sf_sequence(myprop,json_command)
!                case ('thrustplots')
!                    call sf_thrust_plots(myprop,json_command)
                case default
                    write(*,*) 'Command not recognized.'
            end select
        end if
    end do

    call prop_deallocate(myprop)

    call cpu_time(time2)
    write(*,*) 'CPU time total (sec): ',time2-time1

!    filename = 'out.json'
!    call prop_write_json_file(myprop,filename)

end program main
