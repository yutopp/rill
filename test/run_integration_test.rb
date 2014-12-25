cwd = Dir.pwd
test_dir = File.expand_path("integration", File.dirname(__FILE__))

compier_path = File.join(cwd, 'tools/compiler/rillc')
link_rt_lib_path = File.join(cwd, 'rill-rt/librill-rt.a')
link_import_rt_lib_dir = File.join(cwd, '../rill-rt/src')

Dir.chdir(test_dir) do
  Dir.glob('test*.rill').each_with_index do |name, index|
    begin
      puts "=========================="
      puts "== RUN: #{name} =========="

      r = system(compier_path,
                 "--rill-link-rt-lib-path=#{link_rt_lib_path}",
                 "--rill-import-rt-lib-dir=#{link_import_rt_lib_dir}",
                 name,
                 {
                   :out => "/dev/null",
                   :err => "/dev/null"
                 }
                )

      unless r
        raise "rillc failed"
      end

      r = system('./a.out')

      puts "== OK ===================="

    rescue => s
      puts "== FAILED ================"

    ensure
      puts ""
    end

  end
end
p cwd, test_dir
#tools/compiler/rillc --rill-link-rt-lib-path=rill-rt/librill-rt.a --rill-import-rt-lib-dir=../rill-rt/src ../tools/compiler/samples/test23.rill
