{application, tke,
 [{description, "Trackeeper"},
  {vsn, "0.1"},
  {modules, [log, tke_appmod, tke, tke_db, tke_hash, tke_rendering_erlang, tke_rendering_html, tke_sup, tke_user]},
  {registered, []},
  {applications, [kernel, stdlib]},
  {mod, {tke,[]}}
 ]}.
