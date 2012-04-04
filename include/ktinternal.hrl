-type http_state() :: 'http_response' | 'http_header' | 'http_body'.
-type response_enc() :: url | base64.

-record(http_state, {
          state             :: http_state(),
          status_code       :: integer(),
          content_length    :: integer(),
          response_enc      :: response_enc(),
          body              :: binary()
         }).

-record(binary_state, { 
          type              :: atom(),
          tot_recs = 0      :: non_neg_integer(),
          n_recvd = 0       :: non_neg_integer(),
          recs = []         :: [#kt_bin_rec{}]
         }).

-type parser_state() :: undefined | #http_state{} | #binary_state{}.

-record(http_response, {
          status_code       :: integer(),
          content_length    :: integer(),
          response_enc      :: response_enc(),
          body              :: binary()
         }).

-record(binary_response, { 
          type              :: atom(),
          tot_recs = 0      :: integer(),
          n_recvd           :: integer(),
          recs              :: [#kt_bin_rec{}]
         }).
