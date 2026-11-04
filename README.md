# hsbisync

A bidirectional file synchronizer between two directories, written in Haskell.
The design was inspired by [Richie765/psync](https://github.com/Richie765/psync);
the [`psync.pl`](psync.pl) in this repository is that project's script (the local
copy has been simplified and lint-fixed, so it is not byte-identical to upstream).

What follows is an English translation of the manual printed by `hsbisync help`
(the tool itself prints it in Chinese).

```text
NAME
     hsbisync - bidirectional file synchronizer between two directories

SYNOPSIS
     hsbisync sync <profile> [--jail <root>]... [--no-confirm]
     hsbisync sync <profile> <root1> <root2> [--jail <root>]...
                 [--no-confirm]
     hsbisync jail <profile>
     hsbisync unjail <profile>
     hsbisync diff <profile>
     hsbisync profile
     hsbisync help | --help

DESCRIPTION
     Keeps two directories identical in both directions: local to local, or
     local to a remote host over SSH. The remote side only needs an executable
     hsbisync on its PATH; the local side starts it through ssh and it acts as
     a helper speaking a binary message protocol on stdin/stdout.

SUBCOMMANDS
     help
         Print this manual; --help is equivalent and byte-identical. Extra
         arguments are a usage error.

     sync <profile>
         Read the peer address recorded in .bisync/<profile>/repo and sync the
         current directory. Both sync roots must already exist: no directories
         are created and the repo is not rewritten.

     sync <profile> <root1> <root2>
         Sync two roots given explicitly. A missing sync root is created one
         level deep (its parent must already exist); the peer address is
         written to .bisync/<profile>/repo on both ends, so the argument-less
         form can be used afterwards.

         Option --jail <root> (repeatable) only makes sure that end has an
         empty link_table.json (nothing happens if it already exists) so the
         link phase compares against the table. It does not move any links;
         use the jail / unjail subcommands for that. An end not named by
         --jail is left untouched. It is a switch, so the path stays a
         positional argument:
             sync p --jail A B
             sync p A --jail B

         Option --no-confirm: answer y at every confirmation (scripts, no tty).

     jail <profile>
         Turn the current directory into a jail repository: existing symlinks
         move into link_table.json and are unlinked; the link target's content
         is then synced as real directories at the same relative path.

     unjail <profile>
         The reverse: the table's entries are recreated as real symlinks
         (whatever occupies that path is removed first; the content is
         re-synced), then the table is deleted.

     diff <profile>
         Read-only preview: changes of the current directory relative to the
         last baseline, with the dimension that changed (size, old and new
         mtime; times shown in the local timezone). No state is written.

     profile
         List every profile under .bisync/ together with its repo and ignore
         contents. Read-only, takes no arguments.

     --helper --passcode=dontrunyourself --root=<path> [--tag=<profile>]
         Internal: the remote half of the process, started locally over ssh.
         --passcode guards against running it by hand.

ROOTS
     Two forms of root address:
         /local/path
         [user@]host:/remote/path

JAIL REPOSITORIES
     A jail end keeps symlinks only in .bisync/<profile>/link_table.json and
     creates none on disk; the link target's content is synced as ordinary
     files/directories at the same relative path - which is what a host that
     should just receive config content needs.

     The table is one line of JSON keyed by relative path; empty is {}:
         {"conf":{"target":"/home/a/.config/x","size":19,
                  "mtime":1758300001}}

     size is the link's st_size and mtime the link's own mtime - the link
     phase's comparison key, which a jail end has no lstat to obtain. A
     malformed table is an error, never treated as empty.

     --jail works on a remote root too: the helper creates the empty table
     there (and refuses if that end still has real links). The jail / unjail
     conversion subcommands act on the local current directory only.

SYNC PROCEDURE
     A sync runs in 6 steps:

     [1/6] Sync prepare ...
         Roots -> handshake -> repo.
         Roots: in explicit-address mode a missing root is created (one level
         only); in argument-less mode existence is only checked.
         Handshake: ssh over, obtain the absolute path with
         `command -v hsbisync`, then start the helper; the helper reports its
         own executable path and home directory, and every subsequent remote
         call uses that absolute path. The repo is written on both ends only in
         explicit-address mode.

     [2/6] Syncing ignore rules ...
         Unconditionally align .bisync/<profile>/ignore on both ends: the newer
         side overwrites the older one. This must come first, so that the file
         sets scanned on both ends are comparable. After this step the ignore
         file no longer takes part in file syncing.

     [3/6] Syncing symlinks ...
         Only the link's own lstat is compared (its length is the length of the
         target string, plus the link's own mtime); the link is not followed.
         A jail end has no lstat: the link set and both values come from
         link_table.json. If the link structures on the two ends do not agree
         (unresolved conflicts, actions that did not land, or a refusal at the
         confirmation prompt), steps 4 to 6 are skipped entirely. The reason:
         the file phase compares by relative path + (size, mtime), which assumes
         that the same path refers to the same object on both ends; when links
         are not aligned that assumption fails.

         When one end still holds a link but the other end's path is occupied
         by a real file/directory (the link was replaced by content, not
         removed), the link is restored on that end instead of propagating a
         deletion. Only a path that is really gone propagates the deletion.

         A deletion is carried one step further: a link dropped from the
         baseline this run (link_deleted) is also deleted on the peer when the
         peer still holds the same link (same length and mtime) - otherwise the
         relay would see it as "new" on the next run and pull it back. Once the
         peer drops the link, the real files it pointed to are out of scope, so
         they are not deleted.

     [4/6] Reading states ...
         Both ends are scanned in parallel (each filtered by the now-identical
         ignore rules) and compared with their own baseline. The scan is
         depth-first and follows symlinks (a link to a directory is walked as a
         directory); dangling links get no special treatment. Empty directories
         are removed along the way with rmdir (never a recursive delete), each
         end cleaning up its own.

     [5/6] Computing sync actions ...
         Comparison key = relative path + size + integer-second mtime; the mode
         does not take part.
         Pseudo-conflicts that resolve automatically:
             both sides new or updated, with the same size and integer-second
             mtime
                 -> considered already in sync, nothing is transferred
             both sides deleted -> only recorded, no files touched
         A deletion propagates only when the other side has not changed; if it
         has changed, that is a conflict. Real conflicts are marked unresolvable
         and left to the user. If there is nothing to do, step 6 is not entered.

         When there is work, files that are about to be transferred or are in
         conflict, and whose size matches on both ends, get an extra content
         check: the SHA-512 of each pair is compared. Equal ones are downgraded
         to record-only, with the older end's mtime aligned to the newer one;
         different ones keep their original verdict.

     [6/6] Sync and record files ...
         Entered only when there is work. The final actions are listed before
         the confirmation; only after confirming are they really transferred and
         deleted, and the new baseline is written. If only unresolvable
         conflicts remain, the confirmation is skipped.
         Writes to disk are atomic: a .hsbisync-tmp<pid>-<seq>.tmp file is
         created in the target's directory, given the right mode and mtime, and
         renamed onto the target name only after the hash check passes.
         Verification happens before the rename; on failure the temporary file
         is discarded and the original file is untouched.

FILES
     .bisync/<profile>/repo
         One line with the peer address. Written automatically by
         sync <profile> <root1> <root2>.

     .bisync/<profile>/ignore
         The ignore rules, gitignore syntax. Aligned on both ends by step 2.

     .bisync/<profile>/link_table.json
         The link table of a jail repository; never synced. Its presence makes
         the end jail; the jail / unjail subcommands switch between forms.

     The three baseline files (one set for the file tree and one for the
     symlink tree, with the same shape):

     .bisync/<profile>/state          snapshot taken at the last sync
     .bisync/<profile>/added          entries added since the last sync (log)
     .bisync/<profile>/deleted        names deleted since the last sync (log)
     .bisync/<profile>/link_state     the same, for the symlink tree
     .bisync/<profile>/link_added
     .bisync/<profile>/link_deleted

     state is the snapshot from the last sync, added / deleted are the deltas
     since then. A snapshot alone cannot tell the direction of a change: a file
     present on one end but absent from the other - was it added or deleted?
     Only a baseline gives a reference, and only then do changes have a
     direction.

     Each profile describes one sync relationship (one peer plus one set of
     ignore rules), and each end keeps its own copy. The .bisync directory
     itself never takes part in syncing; the ignore file is aligned by step 2.

IGNORE RULES
     .bisync/<profile>/ignore filters the file tree at scan time using
     gitignore syntax (implemented by gitignore-ospath): ignored entries are
     invisible to sync - not deleted, not transferred. The ignore file itself is
     aligned on both ends by step 2.

     Hidden files get no special treatment; .git/ and friends are synced as
     well. The .bisync directory itself is always skipped. The tool's temporary
     files (.hsbisync-tmp*) take part in sync like any other file.

EXIT STATUS
     0    Finished normally. Includes Nothing to do., Aborted. printed when the
          user declines at the confirmation, and the case where the symlink
          phase stops with a single line of explanation.
     1    Bad arguments, or a pre-flight check failed.
     Diagnostics go to standard output in the form Error: <message>.

ENVIRONMENT
     The process is pinned to UTF-8 (file names, arguments, contents, handles),
     independent of LANG and LC_ALL, so the local side and the remote helper
     behave identically.

LIMITATIONS
     Remote requirements: passwordless ssh, and a same-version hsbisync on the
     login shell's PATH.

     Conflicts: if both sides changed the same file it is a conflict - no
     automatic merge, only a list for a human to handle. No history is kept, so
     there is no rollback.

     File transfer: payloads are streamed in 64 KiB chunks - read while
     sending, written while receiving - so peak memory depends on the chunk
     size, not on the file size. No delta transfer, and no resumable transfer
     either (a rerun after an interruption starts over from the beginning).
     Temporary files left by an interruption are not cleaned up automatically
     and may be synced as ordinary files. No fsync.

EXAMPLES
     First sync:
         hsbisync sync local xx-cli user@host:project/xx-cli

     Afterwards:
         hsbisync sync local

     Inspect changes:
         hsbisync diff local

     List all profiles:
         hsbisync profile

     Turn the current directory into a jail repository:
         hsbisync jail local

     Turn it back into a normal repository:
         hsbisync unjail local
```
