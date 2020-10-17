# keep-alive

 This module allows you to set per-connection keep alive parameters on windows and linux enviroments.
 For more information on keep alive signals see <https://en.wikipedia.org/wiki/Keepalive>.
 See also <https://tldp.org/HOWTO/html_single/TCP-Keepalive-HOWTO/> for a linux specific implementation.

 The module is meant to be used in conjuction with the "network" package. Speciffically, all functions require
 a socket file descriptor argument which can be obtained by the withFdSocket function:

```hs
 -- sock is a Socket type
 withFdSocket sock $ \fd do
    before <- getKeepAliveOnOff fd
    print before -- False
    -- set keep alive on, idle 60 seconds, interval 2 seconds
    rlt <- setKeepAlive fd $ KeepAlive True 60 2
    case rlt of
        Left err -> print err
        Right () -> return ()
    after <- getKeepAliveOnOff fd
    print after -- True
```
