import { intent } from "./event-history-recovery-plans.registration.js";

// Native roots here are observed-input models, not an RPC/CAS simulation. The
// assertions exercise retained SQL operation selection under actual authority;
// fresh chain authorization and the drained native diagnostics belong to callers.
export const retainedIntent = () => {
  const { expectedRoot: candidateRoot, ...value } = intent();
  return { value, candidateRoot };
};
