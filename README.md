# Erlang-BuildSystem
A simple Erlang server Builder implementation, an alternative to the build-system Ninja. Builder
makes it possible to start a build according to a build plan that describes what tasks need to
done to complete a build.

The build result of a build is a pair where the rst component is the result status and is one
of the atoms success or failure; the second component is the result value and can be any
valid Erlang value.
A build plan describes the different sub-builds of a build, where a sub-build is also described
by a build plan.

The following build plan show how to run two commands in sequence:
{seq, [ {act, write_to_file, ["hello.txt", "Can we fix it?\n"]}
, {act, write_to_file, ["hello.txt", "Yes, we can!\n"]}
]}
Where write_to_file could be an action used for writing text to a file.
