{application
, chatroom
, [{description, "chat room for test"}
  , {vsn, "0.1"}
  , {modules, [chatroom_app, chatroom_sup, chatroom_ctr, chatroom_ser]}
  , {registered, [chatroom_ser]}
  , {applications, [kernel, stdlib]}
  , {mod, {chatroom_app, []}}
  ]
}.
