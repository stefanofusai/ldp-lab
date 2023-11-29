-module(test).
-export([test/0]).

test() ->
    joseph:joseph(30, 3),
    joseph:joseph(300, 1001),
    joseph:joseph(3000, 37),
    joseph:joseph(26212, 2025),
    joseph:joseph(1000, 1000),
    joseph:joseph(2345, 26212),
    joseph:joseph(100000, 7).

% Expected output:
% In a circle of 30 people, killing number 3
% Joseph is the Hebrew in position 29
% In a circle of 300 people, killing number 1001
% Joseph is the Hebrew in position 226
% In a circle of 3000 people, killing number 37
% Joseph is the Hebrew in position 1182
% In a circle of 26212 people, killing number 2025
% Joseph is the Hebrew in position 20593
% In a circle of 1000 people, killing number 1000
% Joseph is the Hebrew in position 609
% In a circle of 2345 people, killing number 26212
% Joseph is the Hebrew in position 1896
% In a circle of 100000 people, killing number 7
% Joseph is the Hebrew in position 27152
