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
} as const);

const FIXED_SCRIPT_CATALOGUE =
  verifyDeploymentManifestV1FraudProofCatalogueIdentity({
    root: "015807b1d2fb87e4a04d5085fcc36cf654bb8126d3687051594799221801415b",
    categories: {
      doubleSpend: {
        categoryId: "00000000",
        scriptHash: "bddf4b5c833decbf82201931cffc54f7c7dc51e4e6743a25a95aa2c0",
        membershipProofCbor:
          "9fd8799f005f58406fa3aa4d13788081359c25cc0ba03c3077ced23f838de4beaf0b76dcb45c7e5754d72578de167a69d537ef3491e6ed444adc3df26db7f28d94d53007e351757f58400eb923b0cbd24df54401d998531feead35a47a99f4deed205de4af81120f97610000000000000000000000000000000000000000000000000000000000000000ffffff",
      },
      nonExistentInput: {
        categoryId: "00000001",
        scriptHash: "bddf4b5c833decbf82201931cffc54f7c7dc51e4e6743a25a95aa2c0",
        membershipProofCbor:
          "9fd8799f005f584073595758f9806ce4b1e0aca22146c62b1ad4dadbe12512f8a4ed96994c7e034f56827e97f383699af00c904f3847e46e0eb329b2f0946b5062376cbbad1a601958400eb923b0cbd24df54401d998531feead35a47a99f4deed205de4af81120f97610000000000000000000000000000000000000000000000000000000000000000ffffd87b9f005820ee008edbcebc51812e09a76640db05559631cf9730f2fddd093fc72d072d323b5820a7be719f5f57828776b40db4e2476aeb846750aa270d80c950d836d48f337850ffff",
      },
      nonExistentInputNoIndex: {
        categoryId: "00000002",
        scriptHash: "bddf4b5c833decbf82201931cffc54f7c7dc51e4e6743a25a95aa2c0",
        membershipProofCbor:
          "9fd8799f005f584073595758f9806ce4b1e0aca22146c62b1ad4dadbe12512f8a4ed96994c7e034f6f6e7565843b178ca5455479111ae382832684b6964410b11ec083f070a8e7155840268dc64c1b55751769c2bc8df6dae3284bc32b4ee88c6e260c77d2684b258bcf0000000000000000000000000000000000000000000000000000000000000000ffffd87b9f0058208df5e96b9134dbf02ce0619afa67c94a9d09abbe241505f82935aef5d3dc39cc5820a7be719f5f57828776b40db4e2476aeb846750aa270d80c950d836d48f337850ffff",
      },
      invalidRange: {
        categoryId: "00000003",
        scriptHash: "bddf4b5c833decbf82201931cffc54f7c7dc51e4e6743a25a95aa2c0",
        membershipProofCbor:
          "9fd8799f005f584073595758f9806ce4b1e0aca22146c62b1ad4dadbe12512f8a4ed96994c7e034f56827e97f383699af00c904f3847e46e0eb329b2f0946b5062376cbbad1a601958400eb923b0cbd24df54401d998531feead35a47a99f4deed205de4af81120f97610000000000000000000000000000000000000000000000000000000000000000ffffd87b9f005820ef9660efe206d50189739680bd866387c07a061a7b5de18f85b6af241f1686385820a7be719f5f57828776b40db4e2476aeb846750aa270d80c950d836d48f337850ffff",
      },
      transitionTrace: {
        categoryId: "00000004",
        scriptHash: "bddf4b5c833decbf82201931cffc54f7c7dc51e4e6743a25a95aa2c0",
        membershipProofCbor:
          "9fd8799f005f584073595758f9806ce4b1e0aca22146c62b1ad4dadbe12512f8a4ed96994c7e034f6f6e7565843b178ca5455479111ae382832684b6964410b11ec083f070a8e7155840268dc64c1b55751769c2bc8df6dae3284bc32b4ee88c6e260c77d2684b258bcf0000000000000000000000000000000000000000000000000000000000000000ffffd87b9f0058208bb58512fc6c43f79b51020d086948648b960d5ee65bb6a9a737e2af545177265820a7be719f5f57828776b40db4e2476aeb846750aa270d80c950d836d48f337850ffff",
      },
      zeroInput: {
        categoryId: "00000005",
        scriptHash: "bddf4b5c833decbf82201931cffc54f7c7dc51e4e6743a25a95aa2c0",
        membershipProofCbor:
          "9fd8799f005f58406fa3aa4d13788081359c25cc0ba03c3077ced23f838de4beaf0b76dcb45c7e57875c44f1c501b9c380dc37af501bb784db08a6c9748bdbd05e5e43eb75a00f1658400eb923b0cbd24df54401d998531feead35a47a99f4deed205de4af81120f97610000000000000000000000000000000000000000000000000000000000000000ffffff",
      },
      validationTraceDispute: {
        categoryId: "00000006",
        scriptHash: "bddf4b5c833decbf82201931cffc54f7c7dc51e4e6743a25a95aa2c0",
        membershipProofCbor:
          "9fd8799f005f584073595758f9806ce4b1e0aca22146c62b1ad4dadbe12512f8a4ed96994c7e034f6f6e7565843b178ca5455479111ae382832684b6964410b11ec083f070a8e7155840d45eddb1c64a9ffe78bfe2e9f34c30ede448d90138915a74c4c7110dc7fed8420000000000000000000000000000000000000000000000000000000000000000ffffff",
      },
    },
  });

const POSITIONAL_SCRIPT_CATALOGUE =
  verifyDeploymentManifestV1FraudProofCatalogueIdentity({
    root: "e8b2f26d5087a065c0798b6cc3e39e00b1ef33edcd6316e4572903c6d7acf167",
    categories: {
      doubleSpend: {
        categoryId: "00000000",
        scriptHash: "9dfd836b784ba4a6d2ae0d834e11290f5b552e651a5f1d32339a5d2d",
        membershipProofCbor:
          "9fd8799f005f584053a920c4e7d9c50a6ff9d2b832d22300d625e205d28e07ff6c15cde2c96667fda4a393aca51215174bcf95d0bf7e78a4f4ab680d548549eecf73ae9f7af5b44458400eb923b0cbd24df54401d998531feead35a47a99f4deed205de4af81120f97610000000000000000000000000000000000000000000000000000000000000000ffffff",
      },
      nonExistentInput: {
        categoryId: "00000001",
        scriptHash: "fa33d919b9072221cab6322dbba0839f89cb70241c01ec7939fb374c",
        membershipProofCbor:
          "9fd8799f005f584096ec1ec09f76df9417465c5e2feca7b4e92c0e940077fad47e342c83c20bd3b3b0bb0cd2674d5e4445866a3cef9e90af43b77c3eceed009032d0a54f9c2f9b3e58400eb923b0cbd24df54401d998531feead35a47a99f4deed205de4af81120f97610000000000000000000000000000000000000000000000000000000000000000ffffd87b9f005820ee008edbcebc51812e09a76640db05559631cf9730f2fddd093fc72d072d323b582087e9235c8d7b3fcb40a3d503037c56a247069ab507548f4b2985965f9b2d2486ffff",
      },
      nonExistentInputNoIndex: {
        categoryId: "00000002",
        scriptHash: "1486de3debbd969c6922c5badd51a94dc959038066a0e2de48b65815",
        membershipProofCbor:
          "9fd8799f005f584096ec1ec09f76df9417465c5e2feca7b4e92c0e940077fad47e342c83c20bd3b3787e609adeb3b9c60ddff678ef9d98b745fb8c4246bb09b88b817f9f882c54085840d37f8fc1042e46e60fbd44c1389af648c2c8d080176d1c08b7d96da99c1b4c840000000000000000000000000000000000000000000000000000000000000000ffffd87b9f0058208df5e96b9134dbf02ce0619afa67c94a9d09abbe241505f82935aef5d3dc39cc582072b7c31107c46a8542b2630c53d8b072282ddc69a8789a00219aa4ce47b51337ffff",
      },
      invalidRange: {
        categoryId: "00000003",
        scriptHash: "7a7b8dfce83085bb6f120f13f01d05ac9cba8672a0fd619692b41912",
        membershipProofCbor:
          "9fd8799f005f584096ec1ec09f76df9417465c5e2feca7b4e92c0e940077fad47e342c83c20bd3b3b0bb0cd2674d5e4445866a3cef9e90af43b77c3eceed009032d0a54f9c2f9b3e58400eb923b0cbd24df54401d998531feead35a47a99f4deed205de4af81120f97610000000000000000000000000000000000000000000000000000000000000000ffffd87b9f005820ef9660efe206d50189739680bd866387c07a061a7b5de18f85b6af241f168638582081fb30bfa77c4706ccf546db90b9dfa7fa88d65a2d62e4d021408d4bd2a99326ffff",
      },
      transitionTrace: {
        categoryId: "00000004",
        scriptHash: "614b06a7a916eea8f2f9ea134dbc016ecd3834fef4d699758bba4e02",
        membershipProofCbor:
          "9fd8799f005f584096ec1ec09f76df9417465c5e2feca7b4e92c0e940077fad47e342c83c20bd3b3787e609adeb3b9c60ddff678ef9d98b745fb8c4246bb09b88b817f9f882c54085840d37f8fc1042e46e60fbd44c1389af648c2c8d080176d1c08b7d96da99c1b4c840000000000000000000000000000000000000000000000000000000000000000ffffd87b9f0058208bb58512fc6c43f79b51020d086948648b960d5ee65bb6a9a737e2af545177265820b2b437710c1902cd213fb2dbfa15b8d8c9a23832a0fe07c17085ea14c4e850fcffff",
      },
      zeroInput: {
        categoryId: "00000005",
        scriptHash: "8fc42e4ae985854afc814e266c609ab93e3f7321e5fd0bf8910f7836",
        membershipProofCbor:
          "9fd8799f005f584053a920c4e7d9c50a6ff9d2b832d22300d625e205d28e07ff6c15cde2c96667fde58c229416d0338fc1db79685df06273179c8a54bcecca2c5800e566bf0f79b458400eb923b0cbd24df54401d998531feead35a47a99f4deed205de4af81120f97610000000000000000000000000000000000000000000000000000000000000000ffffff",
      },
      validationTraceDispute: {
        categoryId: "00000006",
        scriptHash: "878423b5dc1048319f93851687fa1fd20c9eae34200a22198fe349a8",
        membershipProofCbor:
          "9fd8799f005f584096ec1ec09f76df9417465c5e2feca7b4e92c0e940077fad47e342c83c20bd3b3787e609adeb3b9c60ddff678ef9d98b745fb8c4246bb09b88b817f9f882c540858400b7ebe397f2ffb4714c082ac3d1048f1b9c4f760d578b0fbef6a2a824d1a2cd30000000000000000000000000000000000000000000000000000000000000000ffffff",
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
