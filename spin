#!/usr/bin/env ruby
require 'rubygems'
require 'socket'
require 'tempfile'
require 'json'

socket_file = "#{Dir.tmpdir}/spin"

begin
  socket = UNIXSocket.open(socket_file)
  socket.puts ARGV.to_json

  while str = socket.sysread(64)
    print str
  end

rescue EOFError
rescue Errno::ECONNREFUSED
  abort "Connection was refused. Have you started up spin serve yet?"
ensure
  socket.close if socket
end
