structure RPC = struct
  fun rpc (url, args) = 
    let 
      val tempfile = FSUtil.tempfilename "/tmp/rpc"
      val argstr = 
        String.concatWith "&" (map (fn (k, v) => k ^ "=" ^ v) args) 
      val cmd = "wget -q -O " ^ tempfile ^ " '" ^ url ^ "?" ^ argstr ^ "'"
      val _ = OS.Process.system cmd
      val contents = StringUtil.readfile tempfile
      val _ = OS.Process.system ("rm " ^ tempfile)
    in
      contents
    end
end
