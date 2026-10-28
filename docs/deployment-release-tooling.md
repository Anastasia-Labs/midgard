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
and `fundingProfiles`. Funding profiles are optional for every profile: the list
may be empty, and the watcher prices its reservations from the runtime funding
policy either way. A profile that is supplied must be a genuine measurement for
this deployment and funding wallet. Authoring refuses missing public provenance,
incomplete parameter records, mismatched contract bytes or references, a
supplied profile that belongs to another deployment or wallet, and reuse of a
saved authority for changed release artifacts.

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
  --profile preprod-public \
  --release-blueprint /absolute/release/plutus.json \
  --output /absolute/new-evidence-directory
```

The command archives that exact commit twice, installs the pinned pnpm lock,
executes the existing profile build producer in both fresh directories, requires
nonzero unique validator counts, and compares the complete blueprint bytes.
It builds with the aiken that commit's `demo/flake.lock` pins and retains
source archives, profile identities, raw logs and a receipt.
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

## SDK package publish

The npm publish set is `@al-ft/midgard-core`, `@al-ft/midgard-validation`,
`@al-ft/midgard-sdk` and `@al-ft/lucid-midgard`. The owner publishes them at a
release candidate; these steps are prepared and have not been run.

1. Set one exact version in all four `package.json` files on the release commit.
2. Build the four packages from a clean install.
3. Check the publish set:

   ```sh
   node scripts/release/sdk-packages.mjs --root /absolute/repository \
     --output /absolute/new-check-directory
   ```

   The check packs each package, refuses workspace, file, link, git and URL
   dependency specifiers, dependencies on unpublished workspace packages, tests,
   fixtures, key material and hidden files (such as the local build stamps in
   `dist/`) in a tarball, and exports that do not resolve. It
   installs the tarballs together, imports and requires every export by package
   name, and type-checks both module modes. It exits nonzero and lists each
   problem until the set is publishable. Each run writes `receipt.json` with
   the tarball SHA-256 digests; publish only from a receipt whose status is
   `passed`.

   Each package's `publishConfig` holds the registry form of its `exports`:
   the workspace-only `midgard-source` condition and test-only exports are
   dropped, and each `require` target gets the `.d.cts` declarations the build
   emits beside it. The check refuses a packed export that is not the registry
   form of its workspace export, so change the two together. Core publishes
   no `imports`: its build inlines the `src` files they name. A package's
   `prepack` and `postpack` run [pack-files.mjs](../scripts/release/pack-files.mjs),
   which puts the repository `LICENSE` in the package only while it is packed.
   Validation imports its Merkle Patricia Forestry as
   `#merkle-patricia-forestry`. In the workspace that names the package
   installed from [the vendored tarball](../demo/vendor/README.md); for the pack
   the script copies the installed package to `vendor/merkle-patricia-forestry/`,
   where validation's published `imports` entry points, so the registry package
   runs the vendored code beside its MPL-2.0 licence. The check compares that
   copy byte for byte with the installed package and requires its dependencies
   in validation's own.

4. Publish exactly the checked tarballs, in dependency order: core, validation,
   SDK, then lucid-midgard. Compare each digest with the receipt first;
   repacking does not reproduce identical bytes.

   ```sh
   npm publish /absolute/new-check-directory/tarballs/<tarball>.tgz --access public
   ```

5. Install the published versions into an empty project and import each package
   by name before announcing the release.
