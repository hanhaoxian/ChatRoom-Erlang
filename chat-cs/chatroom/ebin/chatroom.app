{application,chatroom,
             [{description,"chat room for test"},
              {vsn,"0.1"},
              {modules,[cr_app,cr_ctr,cr_ser,cr_sup]},
              {registered,[cr_ser]},
              {applications,[kernel,stdlib]},
              {mod,{cr_app,[]}}]}.
