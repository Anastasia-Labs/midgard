import {
  DEPLOYMENT_MANIFEST_V1_FRAUD_PROOF_CATALOGUE_CATEGORY_ORDER,
  type DeploymentManifestV1FraudProofCatalogueIdentity,
  verifyDeploymentManifestV1FraudProofCatalogueIdentity,
} from "../../midgard-core/src/deployment-manifest-identity-v1.js";

type ContractIdentity = Readonly<{ scriptHash: string }>;

const CONTRACT_BY_CATEGORY = Object.freeze({
  doubleSpend: "fraudProofDoubleSpend",
  nonExistentInput: "fraudProofNonExistentInput",
  nonExistentInputNoIndex: "fraudProofNonExistentInputNoIndex",
  invalidRange: "fraudProofInvalidRange",
  transitionTrace: "fraudProofTransitionTrace",
  zeroInput: "fraudProofZeroInput",
  validationTraceDispute: "validationTraceDispute",
  daHashPreimage: "fraudProofDaHashPreimage",
  noReferenceInput: "fraudProofNoReferenceInput",
  referenceInputNoIdx: "fraudProofReferenceInputNoIdx",
  invalidSignature: "fraudProofInvalidSignature",
} as const);

const FIXED_SCRIPT_CATALOGUE =
  verifyDeploymentManifestV1FraudProofCatalogueIdentity({
    root: "b5a265573c875c48a999adb3672a77c070a0511e1662e406ec8c7d81b8689134",
    categories: {
      doubleSpend: {
        categoryId: "00000000",
        scriptHash: "bddf4b5c833decbf82201931cffc54f7c7dc51e4e6743a25a95aa2c0",
        membershipProofCbor:
          "9fd8799f005f58406c68a2a4f4f27ea8d142298bd80d981c5f1d3b59b9a0350daebcb33d9208c28ddcfcfab97aaa44dc41df27d64e820305294530e4fe9f91d8123b6a25ea2219bb58400eb923b0cbd24df54401d998531feead35a47a99f4deed205de4af81120f97610000000000000000000000000000000000000000000000000000000000000000ffffd87b9f0058202dc3142460b6ec3760730ad389b82dd8fc365bcb606458d640c813a4fa4543d45820a7be719f5f57828776b40db4e2476aeb846750aa270d80c950d836d48f337850ffff",
      },
      nonExistentInput: {
        categoryId: "00000001",
        scriptHash: "bddf4b5c833decbf82201931cffc54f7c7dc51e4e6743a25a95aa2c0",
        membershipProofCbor:
          "9fd8799f005f5840a6a7e1bb1258694956a6ba73b622f9b8d71c5f050195154c1f1618c99118519d25eed1d22dc811002d5ba3054e53d7e3f413e4b2881894176b3fe780e9e1e5a85840c04222813484c756a270f64bae84fd4a45e6057ef56152c80f0d8bc654d2bcfb0000000000000000000000000000000000000000000000000000000000000000ffffd87b9f005820ee008edbcebc51812e09a76640db05559631cf9730f2fddd093fc72d072d323b5820a7be719f5f57828776b40db4e2476aeb846750aa270d80c950d836d48f337850ffff",
      },
      nonExistentInputNoIndex: {
        categoryId: "00000002",
        scriptHash: "bddf4b5c833decbf82201931cffc54f7c7dc51e4e6743a25a95aa2c0",
        membershipProofCbor:
          "9fd8799f005f5840a6a7e1bb1258694956a6ba73b622f9b8d71c5f050195154c1f1618c99118519d1ae7640097a0021bd299b4eec6122fbff160e7ce080385c39ff0637d0928a5df5840ad77f6ea9287f8d6db4368967b65bb0817db64d29f1d75d122d7d7395f9ba1a60000000000000000000000000000000000000000000000000000000000000000ffffd87b9f0058208df5e96b9134dbf02ce0619afa67c94a9d09abbe241505f82935aef5d3dc39cc5820a7be719f5f57828776b40db4e2476aeb846750aa270d80c950d836d48f337850ffff",
      },
      invalidRange: {
        categoryId: "00000003",
        scriptHash: "bddf4b5c833decbf82201931cffc54f7c7dc51e4e6743a25a95aa2c0",
        membershipProofCbor:
          "9fd8799f005f5840a6a7e1bb1258694956a6ba73b622f9b8d71c5f050195154c1f1618c99118519d25eed1d22dc811002d5ba3054e53d7e3f413e4b2881894176b3fe780e9e1e5a85840c04222813484c756a270f64bae84fd4a45e6057ef56152c80f0d8bc654d2bcfb0000000000000000000000000000000000000000000000000000000000000000ffffd87b9f005820ef9660efe206d50189739680bd866387c07a061a7b5de18f85b6af241f1686385820a7be719f5f57828776b40db4e2476aeb846750aa270d80c950d836d48f337850ffff",
      },
      transitionTrace: {
        categoryId: "00000004",
        scriptHash: "bddf4b5c833decbf82201931cffc54f7c7dc51e4e6743a25a95aa2c0",
        membershipProofCbor:
          "9fd8799f005f5840a6a7e1bb1258694956a6ba73b622f9b8d71c5f050195154c1f1618c99118519d1ae7640097a0021bd299b4eec6122fbff160e7ce080385c39ff0637d0928a5df5840ad77f6ea9287f8d6db4368967b65bb0817db64d29f1d75d122d7d7395f9ba1a60000000000000000000000000000000000000000000000000000000000000000ffffd87b9f0058208bb58512fc6c43f79b51020d086948648b960d5ee65bb6a9a737e2af545177265820a7be719f5f57828776b40db4e2476aeb846750aa270d80c950d836d48f337850ffff",
      },
      zeroInput: {
        categoryId: "00000005",
        scriptHash: "bddf4b5c833decbf82201931cffc54f7c7dc51e4e6743a25a95aa2c0",
        membershipProofCbor:
          "9fd8799f005f58406c68a2a4f4f27ea8d142298bd80d981c5f1d3b59b9a0350daebcb33d9208c28d8476d066f488ae1a81a182c62b717083e50cdc523255e84416a17797ead539ec58400eb923b0cbd24df54401d998531feead35a47a99f4deed205de4af81120f9761289ceb19f2805a0a26806adfd3ea28927aa9f9548145b4d92f52c8ba31e64b7affffff",
      },
      validationTraceDispute: {
        categoryId: "00000006",
        scriptHash: "bddf4b5c833decbf82201931cffc54f7c7dc51e4e6743a25a95aa2c0",
        membershipProofCbor:
          "9fd8799f005f5840a6a7e1bb1258694956a6ba73b622f9b8d71c5f050195154c1f1618c99118519d1ae7640097a0021bd299b4eec6122fbff160e7ce080385c39ff0637d0928a5df5840d45eddb1c64a9ffe78bfe2e9f34c30ede448d90138915a74c4c7110dc7fed8420000000000000000000000000000000000000000000000000000000000000000ffffd87b9f005820b9df94ec34957f7cb56fdf5b9bb762c2356174d37bc18884d07f7912e1e911135820a7be719f5f57828776b40db4e2476aeb846750aa270d80c950d836d48f337850ffff",
      },
      daHashPreimage: {
        categoryId: "00000007",
        scriptHash: "bddf4b5c833decbf82201931cffc54f7c7dc51e4e6743a25a95aa2c0",
        membershipProofCbor:
          "9fd8799f005f58406c68a2a4f4f27ea8d142298bd80d981c5f1d3b59b9a0350daebcb33d9208c28ddcfcfab97aaa44dc41df27d64e820305294530e4fe9f91d8123b6a25ea2219bb58400eb923b0cbd24df54401d998531feead35a47a99f4deed205de4af81120f97610000000000000000000000000000000000000000000000000000000000000000ffffd87b9f00582023f14560688b1f176c498bd359a2cd02d67082f59614f39ef984cfb47fe8699d5820a7be719f5f57828776b40db4e2476aeb846750aa270d80c950d836d48f337850ffff",
      },
      noReferenceInput: {
        categoryId: "00000008",
        scriptHash: "bddf4b5c833decbf82201931cffc54f7c7dc51e4e6743a25a95aa2c0",
        membershipProofCbor:
          "9fd8799f005f5840a6a7e1bb1258694956a6ba73b622f9b8d71c5f050195154c1f1618c99118519d1ae7640097a0021bd299b4eec6122fbff160e7ce080385c39ff0637d0928a5df5840d45eddb1c64a9ffe78bfe2e9f34c30ede448d90138915a74c4c7110dc7fed8420000000000000000000000000000000000000000000000000000000000000000ffffd87b9f005820ba0ca986830632d31981be61644386b7d1206a0a3b806368b80c3449633672045820a7be719f5f57828776b40db4e2476aeb846750aa270d80c950d836d48f337850ffff",
      },
      referenceInputNoIdx: {
        categoryId: "00000009",
        scriptHash: "bddf4b5c833decbf82201931cffc54f7c7dc51e4e6743a25a95aa2c0",
        membershipProofCbor:
          "9fd8799f005f58406c68a2a4f4f27ea8d142298bd80d981c5f1d3b59b9a0350daebcb33d9208c28d8476d066f488ae1a81a182c62b717083e50cdc523255e84416a17797ead539ec58400eb923b0cbd24df54401d998531feead35a47a99f4deed205de4af81120f97618477b3a47a28a0da0c8f592df1055d6afae1e64fe456b3030062d47af8b1ffe9ffffff",
      },
      invalidSignature: {
        categoryId: "0000000a",
        scriptHash: "bddf4b5c833decbf82201931cffc54f7c7dc51e4e6743a25a95aa2c0",
        membershipProofCbor:
          "9fd8799f005f5840a6a7e1bb1258694956a6ba73b622f9b8d71c5f050195154c1f1618c99118519d25eed1d22dc811002d5ba3054e53d7e3f413e4b2881894176b3fe780e9e1e5a85840cb4cbd809e8abb415df606bb52334d9bd8d5eb6ed33c8256a6b5f186fd80bf350000000000000000000000000000000000000000000000000000000000000000ffffff",
      },
    },
  });

// Re-derived 2026-08-14 (#579). These are NOT blueprint script hashes: the
// suites that load this fixture build one synthetic single-byte script per
// contract, keyed on the contract's POSITION in
// `DEPLOYMENT_MANIFEST_V1_CONTRACT_NAMES` (see proof-thread-indexer.test.ts:207
// — script = `(index + 1).toString(16).padStart(2, "0")`). #579 removed THREE
// retired tx-field names from that positional vector —
// `txOrderFieldPreimageSpend`, `txOrderFieldReceiptSpend` and
// `txOrderFieldReceiptMint` — which shifted every fraud-proof contract three
// places down and so moved all eleven synthetic hashes and the folded catalogue
// root with them. Nothing about the fraud-proof families themselves changed;
// this is purely the renumbering, which is exactly why a positional vector's
// removals have to be enumerated rather than assumed harmless.
//
// Derived by running the deployment producer itself
// (`buildFraudProofCatalogueDeploymentInfo(fraudProofsToIndexedValidators(...))`,
// demo/midgard-node/src/transactions/initialization.ts) over the same synthetic
// contract set the suites build — never transcribed from a failing assertion.
// Root: e6462b63… -> 9e71465d… (receipt pair) -> dc6d920a… (preimage lock).
const POSITIONAL_SCRIPT_CATALOGUE =
  verifyDeploymentManifestV1FraudProofCatalogueIdentity({
    root: "dc6d920a1e6d2e94e14ea147ad6bedaaba6148b8199afc543ffc7f26e2c1bb26",
    categories: {
      doubleSpend: {
        categoryId: "00000000",
        scriptHash: "9c294dd20e9bc9eda66208e9a7069b5f5dbcbabcc302f77af24a1ebb",
        membershipProofCbor:
          "9fd8799f005f584073528607b66dad81b2eaffde0eeaf223bcaa06cf44dc013bdef9a375929cdef9b9aeb9d3d1c425a9d9c53096a6cf084c47885d4abbd7567d33bc78b4954e057e58400eb923b0cbd24df54401d998531feead35a47a99f4deed205de4af81120f97610000000000000000000000000000000000000000000000000000000000000000ffffd87b9f0058202dc3142460b6ec3760730ad389b82dd8fc365bcb606458d640c813a4fa4543d4582038567247b400a901039b760a5ed77bdae87f471b72e8ba82f41af329f12839ecffff",
      },
      nonExistentInput: {
        categoryId: "00000001",
        scriptHash: "394ec87928dca91a6f0ae9b7395e12c9dadb6e03c1c3999826f590b3",
        membershipProofCbor:
          "9fd8799f005f5840c8cf60bab3857c7712685177c329663182fa12fd29ae8e2c049a4a684f611093306f7ea87cd59f8e013ad34d7707bc0c94781a8e7d6a9eeb55b1422ad997cee55840f9f09699d2fd12129f90c6febc96b713690b08293ea18bb60b2b9139d86275620000000000000000000000000000000000000000000000000000000000000000ffffd87b9f005820ee008edbcebc51812e09a76640db05559631cf9730f2fddd093fc72d072d323b5820ae7bd279d277b52b78d6f35c7b55cd6ce09a213209684b0ba29757976351dbebffff",
      },
      nonExistentInputNoIndex: {
        categoryId: "00000002",
        scriptHash: "3c7cf1b727a2e1b8e42c991aea3dcf4576c45165a1ec87cc646c3be4",
        membershipProofCbor:
          "9fd8799f005f5840c8cf60bab3857c7712685177c329663182fa12fd29ae8e2c049a4a684f611093e8aa125b36d1257020b01c7704b83d9539ee4b82f0eb55810d7576a15c177ac058409392e39d72b20c24063b8c4a5dc932b224784aecd29085b639f46c4bd5bdb7b60000000000000000000000000000000000000000000000000000000000000000ffffd87b9f0058208df5e96b9134dbf02ce0619afa67c94a9d09abbe241505f82935aef5d3dc39cc582081fb30bfa77c4706ccf546db90b9dfa7fa88d65a2d62e4d021408d4bd2a99326ffff",
      },
      invalidRange: {
        categoryId: "00000003",
        scriptHash: "9dfd836b784ba4a6d2ae0d834e11290f5b552e651a5f1d32339a5d2d",
        membershipProofCbor:
          "9fd8799f005f5840c8cf60bab3857c7712685177c329663182fa12fd29ae8e2c049a4a684f611093306f7ea87cd59f8e013ad34d7707bc0c94781a8e7d6a9eeb55b1422ad997cee55840f9f09699d2fd12129f90c6febc96b713690b08293ea18bb60b2b9139d86275620000000000000000000000000000000000000000000000000000000000000000ffffd87b9f005820ef9660efe206d50189739680bd866387c07a061a7b5de18f85b6af241f1686385820be9292844c478407ac234010335515c74c73e41872c9d72b5b88342cb9a4931affff",
      },
      transitionTrace: {
        categoryId: "00000004",
        scriptHash: "fa33d919b9072221cab6322dbba0839f89cb70241c01ec7939fb374c",
        membershipProofCbor:
          "9fd8799f005f5840c8cf60bab3857c7712685177c329663182fa12fd29ae8e2c049a4a684f611093e8aa125b36d1257020b01c7704b83d9539ee4b82f0eb55810d7576a15c177ac058409392e39d72b20c24063b8c4a5dc932b224784aecd29085b639f46c4bd5bdb7b60000000000000000000000000000000000000000000000000000000000000000ffffd87b9f0058208bb58512fc6c43f79b51020d086948648b960d5ee65bb6a9a737e2af545177265820b89b5f2f985a17a678d97493641f39bd796d427b5940099b27a616fe5875bd20ffff",
      },
      zeroInput: {
        categoryId: "00000005",
        scriptHash: "1486de3debbd969c6922c5badd51a94dc959038066a0e2de48b65815",
        membershipProofCbor:
          "9fd8799f005f584073528607b66dad81b2eaffde0eeaf223bcaa06cf44dc013bdef9a375929cdef9a3a22c17ea2ae7d7b0fc136438e364d6dfb91994957bb1e1a6a2955cc3580f3e58400eb923b0cbd24df54401d998531feead35a47a99f4deed205de4af81120f9761b8efeba06fe066ebea7cedbeac6f10c29315e24b7dcb72191b21b6458992bb8effffff",
      },
      validationTraceDispute: {
        categoryId: "00000006",
        scriptHash: "7a7b8dfce83085bb6f120f13f01d05ac9cba8672a0fd619692b41912",
        membershipProofCbor:
          "9fd8799f005f5840c8cf60bab3857c7712685177c329663182fa12fd29ae8e2c049a4a684f611093e8aa125b36d1257020b01c7704b83d9539ee4b82f0eb55810d7576a15c177ac058404d52e0fa7fa582d30de2aca75447e673036703fb8c0d8855555663564c4c42f20000000000000000000000000000000000000000000000000000000000000000ffffd87b9f005820b9df94ec34957f7cb56fdf5b9bb762c2356174d37bc18884d07f7912e1e9111358201fcb614df741c5f63653e043f254aefc44d8fdbe18491d99a641696ad53b262cffff",
      },
      daHashPreimage: {
        categoryId: "00000007",
        scriptHash: "69bdfdab36b50823d1d4007b02be37134b2ab1b865066e8d1d3e6e04",
        membershipProofCbor:
          "9fd8799f005f584073528607b66dad81b2eaffde0eeaf223bcaa06cf44dc013bdef9a375929cdef9b9aeb9d3d1c425a9d9c53096a6cf084c47885d4abbd7567d33bc78b4954e057e58400eb923b0cbd24df54401d998531feead35a47a99f4deed205de4af81120f97610000000000000000000000000000000000000000000000000000000000000000ffffd87b9f00582023f14560688b1f176c498bd359a2cd02d67082f59614f39ef984cfb47fe8699d5820e3dcaaf6a55e6c90c169e617905102f73c4a8552a2461555c67b510d28f9e0d7ffff",
      },
      noReferenceInput: {
        categoryId: "00000008",
        scriptHash: "4825f9cbd0477cb8aae650a39a0d9264d0c6b89a07023e0dbcc849f6",
        membershipProofCbor:
          "9fd8799f005f5840c8cf60bab3857c7712685177c329663182fa12fd29ae8e2c049a4a684f611093e8aa125b36d1257020b01c7704b83d9539ee4b82f0eb55810d7576a15c177ac058404d52e0fa7fa582d30de2aca75447e673036703fb8c0d8855555663564c4c42f20000000000000000000000000000000000000000000000000000000000000000ffffd87b9f005820ba0ca986830632d31981be61644386b7d1206a0a3b806368b80c344963367204582087e9235c8d7b3fcb40a3d503037c56a247069ab507548f4b2985965f9b2d2486ffff",
      },
      referenceInputNoIdx: {
        categoryId: "00000009",
        scriptHash: "5bfeeac158e74889619ce0f963b8e05c9cb30ac721ce9673099891cb",
        membershipProofCbor:
          "9fd8799f005f584073528607b66dad81b2eaffde0eeaf223bcaa06cf44dc013bdef9a375929cdef9a3a22c17ea2ae7d7b0fc136438e364d6dfb91994957bb1e1a6a2955cc3580f3e58400eb923b0cbd24df54401d998531feead35a47a99f4deed205de4af81120f9761db7649bc8a8a48e43a6854148142d5f3bde357dda9588c160d0946d1dc3e417bffffff",
      },
      invalidSignature: {
        categoryId: "0000000a",
        scriptHash: "216bf7588748dd26962b470586c46e9c069a43167b0ce93cfffe26ea",
        membershipProofCbor:
          "9fd8799f005f5840c8cf60bab3857c7712685177c329663182fa12fd29ae8e2c049a4a684f611093306f7ea87cd59f8e013ad34d7707bc0c94781a8e7d6a9eeb55b1422ad997cee558406b99558eead19428d58b4b0008d34acb70ec564adf2a1c6f2f5000bd4e598d2f0000000000000000000000000000000000000000000000000000000000000000ffffff",
      },
    },
  });

const CATALOGUES = [FIXED_SCRIPT_CATALOGUE, POSITIONAL_SCRIPT_CATALOGUE];

const matchesDeployedScripts = (
  catalogue: DeploymentManifestV1FraudProofCatalogueIdentity,
  contracts: Readonly<Record<string, ContractIdentity>>,
): boolean =>
  DEPLOYMENT_MANIFEST_V1_FRAUD_PROOF_CATALOGUE_CATEGORY_ORDER.every(
    (category) =>
      catalogue.categories[category].scriptHash ===
      contracts[CONTRACT_BY_CATEGORY[category]]?.scriptHash,
  );

export const canonicalFraudProofCatalogueFixture = (
  contracts: Readonly<Record<string, ContractIdentity>>,
): DeploymentManifestV1FraudProofCatalogueIdentity => {
  const catalogue = CATALOGUES.find((candidate) =>
    matchesDeployedScripts(candidate, contracts),
  );
  if (catalogue === undefined) {
    throw new Error(
      "No canonical watcher fraud-proof catalogue fixture matches the deployed scripts",
    );
  }
  return structuredClone(catalogue);
};
