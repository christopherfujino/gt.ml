open Common

let stdlib commit func_name =
  let open Runtime.Commit in
  match func_name with
  | "revision" -> fun _ -> Runtime.String commit.revision
  | "date" -> fun _ -> Runtime.Date commit.committer.date
  | _ -> raise (YoloDawg func_name)
