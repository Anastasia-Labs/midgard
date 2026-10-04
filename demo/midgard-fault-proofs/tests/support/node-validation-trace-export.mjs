// Runtime-only test adapter keeps the node's ambient SQL declarations and
// compiler target out of the fault-proofs TypeScript project. The production
// exporter itself, including its retained witness encoding, is called intact.
export { buildDeterministicValidationTraceMembers } from "midgard-node/mpf/validation-trace";
