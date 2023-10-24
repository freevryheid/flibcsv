program check
  use, intrinsic :: iso_c_binding
  use, intrinsic :: iso_fortran_env, only : stdin=>input_unit, stdout=>output_unit, stderr=>error_unit
  use flibcsv
  implicit none

	integer, parameter :: CHUNK_SIZE = 1024

  type counts
    integer :: fields = 0
    integer :: rows = 0
  end type

  type(csv_parser) :: p
  integer(kind=c_signed_char) :: options
  type(counts), target :: c
  integer(kind=c_size_t) ln
  character(len=:), allocatable :: input
  character(len=2), parameter :: crlf = achar(13) // achar(10)


  ! character(len=:), allocatable :: fin, fout
  ! integer :: nargs, arglen, iostat, u, pos

  ! nargs = command_argument_count()
  ! if(nargs.ne.1) error stop "usage: check csv_file"
  ! call get_command_argument(number=1, length=arglen)
  ! allocate(character(arglen) :: fin)
  ! call get_command_argument(number=1, value=fin, status=iostat)
  ! if (iostat.ne.0) error stop "error: cannot input file path"
  ! fin = trim(adjustl(fin))

  options = 0
  c = counts()
  ! input = "1, 22, 333" // new_line('a') // "4444, 55555, 666666" // new_line('a') // "7777777, 88888888, 999999999"
  input = "1, 22, 333" // crlf // "4444, 55555, 666666" // crlf // "7777777, 88888888, 999999999"
  ln = len(input)

  if(csv_init(p, options).ne.0) error stop "failed to init csv parser"

  if(csv_parse(p, input, ln, c_funloc(cb1), c_funloc(cb2), c_loc(c)).ne.ln) then
    write(stderr, '(2a)'), "Error while parsing string: ", csv_strerror(csv_error(p))
    error stop "aborting"
  endif

  if(csv_fini(p, c_funloc(cb1), c_funloc(cb2), c_loc(c)).ne.0) error stop "failed to finalize parseing"

  call csv_free(p)

  write(stdout, '(a, i0)'), "fields: ", c%fields
  write(stdout, '(a, i0)'), "  rows: ", c%rows

  deallocate(input)

  contains

    subroutine cb1(c1, n1, p1)
      type(c_ptr), value :: c1
      integer(kind=c_size_t), value :: n1
      type(c_ptr), value :: p1
      character(len=:), pointer:: cc1
      type(counts), pointer :: cp1
      call c_f_pointer(c1, cc1)
      call c_f_pointer(p1, cp1)
      cc1 => cc1(1:n1)
      cp1%fields = cp1%fields + 1
      write(stdout, '(a, i0)'), "    n1: ", n1
      write(stdout, '(2a)'),    "   val: ", cc1
    end subroutine cb1

    subroutine cb2(c2, p2)
      integer(kind=c_signed_char), value :: c2
      type(c_ptr), value :: p2
      type(counts), pointer :: cp2
      call c_f_pointer(p2, cp2)
      cp2%rows = cp2%rows + 1
      write(stdout, *), "    c2: ", c2
    end subroutine

end program check

