program check
  use, intrinsic :: iso_c_binding
  use, intrinsic :: iso_fortran_env, only : stdin=>input_unit, stdout=>output_unit, stderr=>error_unit
  use flibcsv
  implicit none

  type counts
    integer :: fields = 0
    integer :: rows = 0
  end type

  type(csv_parser) :: pc
  integer(kind=c_signed_char) :: options
  type(counts), target :: cnt
  integer(kind=c_size_t) ln
  character(len=:), allocatable :: input
  character(len=2), parameter :: CRLF = achar(CSV_CR) // achar(CSV_LF)
  character(len=512) :: msg
  integer, parameter :: CHUNK_SIZE = 1024
  character(len=:), allocatable :: fin, fout
  integer :: nargs, arglen, iostat, u, pos
  logical :: exists

  nargs = command_argument_count()
  if(nargs.ne.1) error stop "usage: check csv_file"
  call get_command_argument(number=1, length=arglen)
  allocate(character(arglen) :: fin)
  call get_command_argument(number=1, value=fin, status=iostat)
  if (iostat.ne.0) error stop "cannot retrieve file path"
  fin = trim(adjustl(fin))

  inquire(file=fin, exist=exists)
  if (.not.exists) error stop "this file not found - " // fin
  open(newunit=u, file=fin, status="old", access="stream", iostat=iostat, iomsg=msg)
  if (iostat.ne.0) error stop trim(msg)




  close(u)

  options = ior(CSV_STRICT, CSV_STRICT_FINI)
  cnt = counts()
  input = "1, 22, 333" // CRLF // "4444, 55555, 666666" // CRLF // "7777777, 88888888, 999999999"
  ln = len(input)

  call csv_set_space_func(pc, c_funloc(is_space))
  call csv_set_term_func(pc, c_funloc(is_term))

  if(csv_init(pc, options).ne.0) &
    error stop "failed to init csv parser"

  if(csv_parse(pc, input, ln, c_funloc(cb1), c_funloc(cb2), c_loc(cnt)).ne.ln) then
    write(stderr, '(2a)'), "Error while parsing string: ", csv_strerror(csv_error(pc))
    error stop "aborting"
  endif

  if(csv_fini(pc, c_funloc(cb1), c_funloc(cb2), c_loc(cnt)).ne.0) &
    error stop "error: failed to finalize parseing"

  call csv_free(pc)

  write(stdout, '(a, i0)'), "fields: ", cnt%fields
  write(stdout, '(a, i0)'), "  rows: ", cnt%rows

  deallocate(input)

  contains

    subroutine cb1(c, n1, p)
      type(c_ptr), value :: c
      integer(kind=c_size_t), value :: n1
      type(c_ptr), value :: p
      character(len=:), pointer:: cc
      type(counts), pointer :: cp
      call c_f_pointer(c, cc)
      call c_f_pointer(p, cp)
      cc => cc(1:n1)
      cp%fields = cp%fields + 1
      write(stdout, '(a, i0)'), "    n1: ", n1
      write(stdout, '(2a)'),    "   val: ", cc
    end subroutine cb1

    subroutine cb2(c, p)
      integer(kind=c_signed_char), value :: c
      type(c_ptr), value :: p
      type(counts), pointer :: cp
      call c_f_pointer(p, cp)
      cp%rows = cp%rows + 1
      write(stdout, *), "    c: ", c
    end subroutine

    function is_space(c) result(ret)
      integer(kind=c_signed_char) :: c
      integer(kind=c_int) :: ret
      if ((c.eq.CSV_SPACE).or.(c.eq.CSV_TAB)) then
        ret = 1
        return
      end if
      ret = 0
    end function is_space

    function is_term(c) result(ret)
      integer(kind=c_signed_char) :: c
      integer(kind=c_int) :: ret
      if ((c.eq.CSV_CR).or.(c.eq.CSV_LF)) then
        ret = 1
        return
      end if
      ret = 0
    end function is_term

end program check

