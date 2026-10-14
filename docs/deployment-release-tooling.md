# Deployment release tooling

A public release uses the existing persistent Ed25519 deployment authority. Its
signed manifest identifies the applied contracts, catalogue, reference records
and protocol profile. The release bindings additionally authenticate a canonical
JSON digest of `release-provenance.json`. This authenticates metadata; the
independent compiler and frozen build evidence establish binary correspondence.
Keep the private signing key outside the repository and provide the public trust
root through a separately controlled file. The roots embedded in an authority
file do not establish trust for public startup.

Public full-stack release inputs add `provenanceFile` and
`parameterRegistryFile` to the existing `signingKeyFile`, `programCommitments`
and genuine measured `fundingProfiles`. Every category installed by the selected
deployment profile needs its measured funding profile. Authoring refuses missing
public provenance, incomplete parameter records, mismatched contract bytes or
references, and reuse of a saved authority for changed release artifacts.

The deployment and reference-publication tools require
`MIDGARD_BLUEPRINT_CLASSIFICATION_FILE` for a public profile. It is an absolute
path to a JSON object mapping every raw blueprint title that the deployment does
not select to an explicit reason. Unknown titles, missing classifications and
deployed titles marked unused are refused. Aiken can emit identical programs
under spend, mint and else titles; classify these exact raw entries explicitly.
For an attached deployment, supply `MIDGARD_DEPLOYMENT_PARAMETER_REGISTRY_FILE`
as the absolute path to its retained registry. The tool independently reapplies
its parameters against the current authenticated blueprint and compares every
actual contract byte/hash/purpose/role before using those records.
The tool records ordered parameter declarations and actual applied CBOR, then
writes `<manifest-path>.blueprint-registry.json` with the finalized manifest and
reference identities. Release authoring independently reapplies those parameters
and binds the complete registry digest in the signed provenance.

The provenance schema is `midgard-deployment-release-provenance-v1`. It carries
`sourceRevision`, `deploymentProfileName`, `deploymentProfileDigest`,
`blueprintHash`, `aikenSourceHash`, `compiler` (version, fork repository/revision
and Cargo lock hash), `roleImages` (repository references with SHA-256 digests),
and `evidence` (canonical JSON digests for the independent blueprint rebuild,
frozen build, and parameter registry). Source revisions are full40 hexadecimal
Git commits. Blueprint hashes are hashes of the actual blueprint bytes.

To compare a separately retained release blueprint with two fresh compiler runs:

```sh
node scripts/release/blueprint-rebuild.mjs \
  --root /absolute/source-repository --revision FULL40_SHA \
  --profile preprod-public --compiler /absolute/pinned/aiken \
  --release-blueprint /absolute/release/plutus.json \
  --output /absolute/new-evidence-directory
```

The command archives that exact commit twice, installs the pinned pnpm lock,
executes the existing profile build producer in both fresh directories, requires
nonzero unique validator counts, and compares the complete blueprint bytes.
It retains source archives, compiler/profile identities, raw logs and a receipt.
A readiness-cache copy is not an independent compiler run. An existing evidence
destination is refused rather than overwritten.

The node, watcher and DA Docker recipes expose `public-final` targets. Build
from the frozen archive's `demo` context with named `release-source` pointing to
that archive root and `onchain` pointing to the verified public blueprint output.
Supply `MIDGARD_DEPLOYMENT_PROFILE`, `MIDGARD_RELEASE_REVISION` and immutable
`MIDGARD_NODE_BUILD_IMAGE`, `MIDGARD_NODE_RUNTIME_IMAGE`,
`MIDGARD_GO_BUILD_IMAGE`, and `MIDGARD_RUST_BUILD_IMAGE` references as build
arguments. The targets run profile generation inside the image build, verify its
source/profile/compiler binding, rebuild the production closure and carry
`/app/release/build-inputs.json`. They use UID/GID10001; the node has an owned
`/app/db` directory for its durable volume. The generic developer target remains
separate. Retain frozen install/build/test receipts and inspect the actual image
contents before assigning release digests; metadata alone does not prove that a
running binary matches source.

Public startup consumes the shared
`@al-ft/midgard-core/deployment-release` verifier. It checks the existing signed
identity against external roots, the mounted finalized manifest and provenance,
and explicit expected source and role-image identities. Missing provenance,
untrusted signers, signature/digest changes, and configuration mismatches are
refused before binding a public listener.
