modules:
begin
  module Com1: PARENT
  module Core: IMPORT
  module Process: IMPORT
  module Mutex: IMPORT
  module Uart: IMPORT
  module Timer: IMPORT
  module System: IMPORT
end

import:
begin
  object watch_reset: Process.Process
  object LOCK_FUN_div: Mutex.mutex
  object watch_set: Process.Process
  object LOCK_FUN_str_to_int: Mutex.mutex
  object LOCK_FUN_str_of_int: Mutex.mutex
  object tty_init: Process.Process
  object LOCK_FUN_tty_print: Mutex.mutex
end

types:
begin
  type Process.Process: {
      method start ()
      method stop ()
      method call ()
    }
  type Mutex.mutex: {
      method init ()
      method lock ()
      method unlock ()
    }
end

code:
begin
               i1_fun: 
                       fun LOCK_FUN_div.init()
           i1_fun_end: 
                       nop
               i2_fun: 
                       fun LOCK_FUN_tty_print.init()
           i2_fun_end: 
                       nop
               i3_fun: 
                       fun LOCK_FUN_str_to_int.init()
           i3_fun_end: 
                       nop
               i4_fun: 
                       fun LOCK_FUN_str_of_int.init()
           i4_fun_end: 
                       nop
               i5_fun: 
                       fun watch_set.start()
           i5_fun_end: 
                       nop
               i6_fun: 
                       fun watch_reset.start()
           i6_fun_end: 
                       nop
               i7_fun: 
                       fun tty_init.start()
           i7_fun_end: 
end
