module flibcsv
  ! libcsv - parse and write csv data
  use, intrinsic :: iso_c_binding
  implicit none
  private

  integer, parameter, public    :: CSV_MAJOR         = 3
  integer, parameter, public    :: CSV_MINOR         = 0
  integer, parameter, public    :: CSV_RELEASE       = 3

  ! Error Codes
  integer, parameter, public    :: CSV_SUCCESS       = 0
  integer, parameter, public    :: CSV_EPARSE        = 1  ! Parse error in strict mode
  integer, parameter, public    :: CSV_ENOMEM        = 2  ! Out of memory while increasing buffer size
  integer, parameter, public    :: CSV_ETOOBIG       = 3  ! Buffer larger than SIZE_MAX needed
  integer, parameter, public    :: CSV_EINVALID      = 4  ! Invalid code,should never be received from csv_error
                                                          ! parser options
  integer, parameter, public    :: CSV_STRICT        = 1  ! enable strict mode
  integer, parameter, public    :: CSV_REPALL_NL     = 2  ! report all unquoted carriage returns and linefeeds
  integer, parameter, public    :: CSV_STRICT_FINI   = 4  ! causes csv_fini to return CSV_EPARSE if last
                                                          ! field is quoted and doesn't containg ending
                                                          ! quote
  integer, parameter, public    :: CSV_APPEND_NULL   = 8  ! Ensure that all fields are null-terminated
  integer, parameter, public    :: CSV_EMPTY_IS_NULL = 16 ! Pass null pointer to cb1 function when
                                                          ! empty, unquoted fields are encountered

  ! Character values
  integer, parameter, public    :: CSV_TAB           = int(z'09')
  integer, parameter, public    :: CSV_SPACE         = int(z'20')
  integer, parameter, public    :: CSV_CR            = int(z'0d')
  integer, parameter, public    :: CSV_LF            = int(z'0a')
  integer, parameter, public    :: CSV_COMMA         = int(z'2c')
  integer, parameter, public    :: CSV_QUOTE         = int(z'22')

  type, bind(c), public         :: csv_parser
    integer(kind=c_int)         :: pstate                 ! Parser state
    integer(kind=c_int)         :: quoted                 ! Is the current field a quoted field?
    integer(kind=c_size_t)      :: spaces                 ! Number of continious spaces after quote or in a non-quoted field
    character(kind=c_char)      :: entry_buf              ! Entry buffer
    integer(kind=c_size_t)      :: entry_pos              ! Current position in entry_buf (and current size of entry)
    integer(kind=c_size_t)      :: entry_size             ! Size of entry buffer
    integer(kind=c_int)         :: status                 ! Operation status
    integer(kind=c_signed_char) :: options
    integer(kind=c_signed_char) :: quote_char
    integer(kind=c_signed_char) :: delim_char
    type(c_ptr)                 :: is_space
    type(c_ptr)                 :: is_term
    integer(kind=c_size_t)      :: blk_size
    type(c_ptr)                 :: malloc_func            ! not used
    type(c_ptr)                 :: realloc_func           ! function used to allocate buffer memory
    type(c_ptr)                 :: free_func              ! function used to free buffer memory
  end type

  public csv_init
  public csv_fini
  public csv_free
  public csv_error
  public csv_strerror
  public csv_parse
  public csv_write
  public csv_fwrite
  public csv_write2
  public csv_fwrite2
  public csv_get_opts
  public csv_set_opts
  public csv_set_delim
  public csv_set_quote
  public csv_get_delim
  public csv_get_quote
  public csv_set_space_func
  public csv_set_term_func
  public csv_set_realloc_func
  public csv_set_free_func
  public csv_set_blk_size
  public csv_get_buffer_size

  ! Function Prototypes
  interface

    ! int csv_init(struct csv_parser *p, unsigned char options);
    function csv_init(p, options) bind(c, name='csv_init') result(res)
      import csv_parser, c_signed_char, c_int
      type(csv_parser) :: p
      integer(kind=c_signed_char), value :: options
      integer(kind=c_int) :: res
    end function csv_init

    ! int csv_fini(struct csv_parser *p, void (*cb1)(void *, size_t, void *), void (*cb2)(int, void *), void *data);
    function csv_fini(p, cb1, cb2, data) bind(c, name='csv_fini') result(res)
      import csv_parser, c_funptr, c_int, c_ptr
      type(csv_parser) :: p
      type(c_funptr), value :: cb1, cb2
      type(c_ptr), value :: data
      integer(kind=c_int) :: res
    end function csv_fini

    ! void csv_free(struct csv_parser *p);
    subroutine csv_free(p) bind(c, name='csv_free')
      import csv_parser
      type(csv_parser) :: p
    end subroutine csv_free

    ! int csv_error(const struct csv_parser *p);
    function csv_error(p) bind(c, name='csv_error') result(res)
      import csv_parser, c_int
      type(csv_parser) :: p
      integer(kind=c_int) :: res
    end function csv_error

    ! const char * csv_strerror(int error);
    function csv_strerror(error) bind(c, name='csv_strerror') result(res)
      import c_char, c_int
      integer(kind=c_int), value :: error
      character(kind=c_char) :: res
    end function csv_strerror

    ! size_t csv_parse(struct csv_parser *p, const void *s, size_t len, void (*cb1)(void *, size_t, void *), void (*cb2)(int, void *), void *data);
    function csv_parse(p, s, len, cb1, cb2, data) bind(c, name='csv_parse') result(res)
      import csv_parser, c_char, c_funptr, c_size_t, c_ptr
      type(csv_parser) :: p
      character(kind=c_char) :: s ! TODO: maybe c_ptr instead
      integer(kind=c_size_t), value :: len
      type(c_funptr), value :: cb1, cb2
      type(c_ptr), value :: data
      integer(kind=c_size_t) :: res
    end function csv_parse

    ! size_t csv_write(void *dest, size_t dest_size, const void *src, size_t src_size);
    ! TODO: ptr vs character. check next func as well
    function csv_write(dest, dest_size, src, src_size) bind(c, name='csv_write') result(res)
      import c_ptr, c_size_t
      type(c_ptr) :: dest, src
      integer(kind=c_size_t), value :: dest_size, src_size
      integer(kind=c_size_t) :: res
    end function csv_write

    ! int csv_fwrite(FILE *fp, const void *src, size_t src_size);
    function csv_fwrite(fp, src, src_size) bind(c, name='csv_fwrite') result(res)
      import c_ptr, c_size_t, c_int
      type(c_ptr), value :: fp
      type(c_ptr) :: src
      integer(kind=c_size_t), value :: src_size
      integer(kind=c_int) :: res
    end function csv_fwrite

    ! size_t csv_write2(void *dest, size_t dest_size, const void *src, size_t src_size, unsigned char quote);
    function csv_write2(dest, dest_size, src, src_size, quote) bind(c, name='csv_write2') result(res)
      import c_ptr, c_size_t, c_signed_char
      type(c_ptr) :: dest, src
      integer(kind=c_size_t), value :: dest_size, src_size
      integer(kind=c_signed_char), value :: quote
      integer(kind=c_size_t) :: res
    end function csv_write2

    ! int csv_fwrite2(FILE *fp, const void *src, size_t src_size, unsigned char quote);
    function csv_fwrite2(fp, src, src_size, quote) bind(c, name='csv_fwrite2') result(res)
      import c_ptr, c_size_t, c_int, c_signed_char
      type(c_ptr), value :: fp
      type(c_ptr) :: src
      integer(kind=c_size_t), value :: src_size
      integer(kind=c_signed_char), value :: quote
      integer(kind=c_int) :: res
    end function csv_fwrite2

    ! int csv_get_opts(const struct csv_parser *p);
    function csv_get_opts(p) bind(c, name='csv_get_opts') result(res)
      import csv_parser, c_int
      type(csv_parser) :: p
      integer(kind=c_int) :: res
    end function csv_get_opts

    ! int csv_set_opts(struct csv_parser *p, unsigned char options);
    function csv_set_opts(p, options) bind(c, name='csv_set_opts') result(res)
      import csv_parser, c_signed_char, c_int
      type(csv_parser) :: p
      integer(kind=c_signed_char), value :: options
      integer(kind=c_int) :: res
    end function csv_set_opts

    ! void csv_set_delim(struct csv_parser *p, unsigned char c);
    subroutine csv_set_delim(p, c) bind(c, name='csv_set_delim')
      import csv_parser, c_signed_char
      type(csv_parser) :: p
      integer(kind=c_signed_char), value :: c
    end subroutine csv_set_delim

    ! void csv_set_quote(struct csv_parser *p, unsigned char c);
    subroutine csv_set_quote(p, c) bind(c, name='csv_set_quote')
      import csv_parser, c_signed_char
      type(csv_parser) :: p
      integer(kind=c_signed_char), value :: c
    end subroutine csv_set_quote

    ! unsigned char csv_get_delim(const struct csv_parser *p);
    function csv_get_delim(p) bind(c, name='csv_get_delim') result(res)
      import csv_parser, c_signed_char
      type(csv_parser) :: p
      integer(kind=c_signed_char) :: res
    end function csv_get_delim

    ! unsigned char csv_get_quote(const struct csv_parser *p);
    function csv_get_quote(p) bind(c, name='csv_get_quote') result(res)
      import csv_parser, c_signed_char
      type(csv_parser) :: p
      integer(kind=c_signed_char) :: res
    end function csv_get_quote

    ! void csv_set_space_func(struct csv_parser *p, int (*f)(unsigned char));
    subroutine csv_set_space_func(p, f) bind(c, name='csv_set_space_func')
      import csv_parser, c_funptr
      type(csv_parser) :: p
      type(c_funptr), value :: f
    end subroutine csv_set_space_func

    ! void csv_set_term_func(struct csv_parser *p, int (*f)(unsigned char));
    subroutine csv_set_term_func(p, f) bind(c, name='csv_set_term_func')
      import csv_parser, c_funptr
      type(csv_parser) :: p
      type(c_funptr), value :: f
    end subroutine csv_set_term_func

    ! void csv_set_realloc_func(struct csv_parser *p, void *(*)(void *, size_t));
    subroutine csv_set_realloc_func(p, f) bind(c, name='csv_set_realloc_func')
      import csv_parser, c_funptr
      type(csv_parser) :: p
      type(c_funptr), value :: f
    end subroutine csv_set_realloc_func

    ! void csv_set_free_func(struct csv_parser *p, void (*)(void *));
    subroutine csv_set_free_func(p, f) bind(c, name='csv_set_free_func')
      import csv_parser, c_funptr
      type(csv_parser) :: p
      type(c_funptr), value :: f
    end subroutine csv_set_free_func

    ! void csv_set_blk_size(struct csv_parser *p, size_t);
    subroutine csv_set_blk_size(p, t) bind(c, name='csv_set_blk_size')
      import csv_parser, c_size_t
      type(csv_parser) :: p
      integer(kind=c_size_t), value :: t
    end subroutine csv_set_blk_size

    ! size_t csv_get_buffer_size(const struct csv_parser *p);
    function csv_get_buffer_size(p) bind(c, name='csv_get_buffer_size') result(res)
      import csv_parser, c_size_t
      type(csv_parser) :: p
      integer(kind=c_size_t) :: res
    end function csv_get_buffer_size

  end interface

  ! contains

end module flibcsv
