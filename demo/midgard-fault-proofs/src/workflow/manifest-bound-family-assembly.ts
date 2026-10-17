/**
 * Manifest-bound family assembly: one module that turns a family definition
 * plus a runtime config into a manifest-bound workflow, and one generic
 * run-or-resume that launches it. Every family used to restate this
 * lifecycle by hand; a fix to the signer assertion, the certificate
 * requirement or the prerequisite order now lands here once.
 *
 * Assembly order, which the table-driven test locks at the interface:
 *   1. bind the deployment with the definition's step datum schemas;
 *   2. assert the signer is the manifest-network enterprise address;
 *   3. require the field-preimage certificate when the definition declares it;
 *   4. bind every reference script (step contract names, then declared
 *      witness roles, then the certificate mint) against the finalized
 *      manifest;
 *   5. open the lazy L1 observation port and require its raw-L1 and
 *      publication authorities;
 *   6. ask the family for its transaction port and replayer, and build the
 *      adapter the definition's arm names: linear, or cursor from the arm's
 *      spec and action refiner;
 *   7. decorate the adapter: field carriage first (in declared order), then
 *      proof chunks, each only when declared;
 *   8. attach the terminal verifier, release-finality authority and any
 *      `extend` members, and freeze.
 *
 * Reference binding precedes provider startup so an offline unit test can
 * assemble every definition, and so a wrong reference fails before a
 * transport is opened.
 */

import "./cursor-family-adapter.js";
import "./deployment-manifest-binding.js";
import "./family-definition.js";
import "./family-l1-observation.js";
import "./field-carriage-prerequisite.js";
import "./linear-family-adapter.js";
import "./orchestrator.js";
import "./proof-chunk-prerequisite.js";
import "./manifest-bound-family-assembly.bind-reference-scripts.js";
import "./manifest-bound-family-assembly.assemble-bound-manifest-bound-family-workflow.js";
import "./manifest-bound-family-assembly.run-or-resume-manifest-bound-family-workflow.js";
export {
  assembleBoundManifestBoundFamilyWorkflow,
  assembleManifestBoundFamilyWorkflow,
  bindManifestBoundFamilyWorkflow,
} from "./manifest-bound-family-assembly.assemble-bound-manifest-bound-family-workflow.js";
export { runOrResumeManifestBoundFamilyWorkflow } from "./manifest-bound-family-assembly.run-or-resume-manifest-bound-family-workflow.js";
