-record(kterl_cursor, {
          cursor_id = 0 :: non_neg_integer(),
          client_pid    :: pid()
         }).

-record(kt_bin_rec, {
          dbidx          :: non_neg_integer(),
          xt             :: non_neg_integer(),
          key            :: binary(),
          val            :: binary()
         }).

-record(kt_http_result, {
          call_type      :: atom(),
          key            :: binary(),
          value          :: binary(),
          exptime        :: non_neg_integer(),
          signaled_count :: non_neg_integer(),
          num            :: number(),
          keys           :: [binary()],
          bulk_records   :: [{Key :: binary(), Value :: binary()}]
         }).
          
          
