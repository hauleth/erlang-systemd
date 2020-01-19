%% Licensed to the Apache Software Foundation (ASF) under one
%% or more contributor license agreements.  See the NOTICE file
%% distributed with this work for additional information
%% regarding copyright ownership.  The ASF licenses this file
%% to you under the Apache License, Version 2.0 (the
%% "License"); you may not use this file except in compliance
%% with the License.  You may obtain a copy of the License at
%%
%%   http://www.apache.org/licenses/LICENSE-2.0
%%
%% Unless required by applicable law or agreed to in writing,
%% software distributed under the License is distributed on an
%% "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
%% KIND, either express or implied.  See the License for the
%% specific language governing permissions and limitations
%% under the License.
%%
-define(SUPERVISOR, systemd_sup).

-define(WATCHDOG, systemd_watchdog).
-define(WATCHDOG_PID, "WATCHDOG_PID").
-define(WATCHDOG_TIMEOUT, "WATCHDOG_USEC").

-define(NOTIFY_SOCKET, "NOTIFY_SOCKET").

-define(LISTEN_FDS_START, 3).
-define(LISTEN_PID, "LISTEN_PID").
-define(LISTEN_FDS, "LISTEN_FDS").
-define(LISTEN_FDNAMES, "LISTEN_FDNAMES").
