# Claude hook adapter

Canonical policy and limitations: [shared harness guidance](../../../doc/ai/harnesses.md).
The Python adapter reads Claude payloads and maps deny/context/prompt to supported Claude responses.
Project settings retain native discovery/wiring. Policy is maintained in
tools/harness/guard_policy.py.

Git mutation decisions are always deny, not ask. The common policy and regression
fixtures cover both clients; see the shared guide for parsing and activation limits.
