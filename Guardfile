guard :shell do
  #
  running_pid = nil
  running_pid_th = nil

  watch( /^(.*?)\.(cpp|hpp|ipp)$/ ) do |m|
    next if File.basename( m[0] ) =~ /^\.#/

    #
    if !running_pid_th.nil? && running_pid_th.alive?
      Process.kill :QUIT, running_pid
      running_pid_th.join
      running_pid = nil
    end
    running_pid_th = nil

    #
    running_pid = Process.fork do
      #
      Signal.trap(:QUIT) do
        puts "Force quitted!"
        exit!
      end

      # DO NOT FORGET TO RUN "cmake" before testing!
      suc = system "make test"

      if ( suc )
        puts "Test Succeeded!"
      else
        puts "Test Failed..."
      end

    end
    running_pid_th = Process.detach( running_pid )
  end
end
