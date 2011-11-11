#!/usr/bin/env ruby
require 'socket'
require 'tempfile'

socket_file = "#{Dir.tmpdir}/spin"

begin
  socket = UNIXSocket.open(socket_file)
  socket.puts ARGV.join(' ')

  while str = socket.sysread(64)
    print str
  end

rescue EOFError
rescue Errno::ECONNREFUSED
  abort "Connection was refused. Have you started up spin serve yet?"
ensure
  socket.close
end
