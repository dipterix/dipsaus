# Current submission (0.1.4):

This version is to patch an issue caused by improperly invoking system commands without checking their existence.

For your convenience, I paste the original email here:

```
On 02/02/2021 08:26, Prof Brian Ripley wrote:
> that is
>
> Rmpi benchmarkme bigparallelr dipsaus disk.frame runjags
>
> On BSD-derived OSes (principally macOS) sysctl is in /usr/sbin or /sbin.
>   Those are areas for *s*ystem commands and you cannot assume they are
> on the path (and they probably should not be).  So your code needs to
> look for them.
>
> If the runjags message
>
> 'Xgrid is not available on this machine.  Xgrid functions are only
> available on machines running Mac OS X 10.5 (Leopard), OS X 10.6 (Snow
> Leopard) or OS X 10.7 (Lion) and with access to an Xgrid controller'
>
> is accurate (and it seems so) this is ancient history and the
> non-functionality should be removed.
>
> Please correct before 2021-02-23 to safely retain your package on CRAN.
>
```

Solution:

* Check if the command `prtconf` (`Solaris`), `sysctl` (`macOS`), `wmic` (`windows`), or `awk` (`linux`) exists on the corresponding operating systems before invoking `system()` calls


Tests:

Tested the updates on `Solaris`, `macOS` (high-sierra with CRAN settings), and `Windows`. The system calls have been properly caught


Additional tests:

* travis (r-release, r-oldrel, and r-devel)
* rhub (windows r-devel, Ubuntu r-release gcc, Fedora r-devel clang)
* win-builder (old, release, devel)
* local macosx (R release)
* local windows (R release, devel)

Check results: 0 error, 0 warning, 0 note
