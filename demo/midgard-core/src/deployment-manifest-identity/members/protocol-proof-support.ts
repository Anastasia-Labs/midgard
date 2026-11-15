import { defineDeploymentFamily } from "../members.types.js";

export const protocolProofSupportMembers = defineDeploymentFamily("protocol", {
  fraudProofCatalogueSpend: {
    purpose: "spend",
    reference: null,
    commands: [],
    validatorTitle: "fraud_proof_catalogue.spend.else",
    familyPaths: [],
  },
  fraudProofCatalogueMint: {
    purpose: "mint",
    reference: {
      role: "fraud-proof-catalogue minting",
      token: "FraudProofCatalogueMint",
    },
    commands: ["protocol-init"],
    validatorTitle: "fraud_proof_catalogue.mint.mint",
    familyPaths: [],
  },
  fraudProofSpend: {
    purpose: "spend",
    reference: null,
    commands: [],
    validatorTitle: "fraud_proof.spend.else",
    familyPaths: [],
  },
  fraudProofMint: {
    purpose: "mint",
    reference: { role: "V1 fraud-proof token minting", token: "V1FpTokenMint" },
    commands: [],
    validatorTitle: "fraud_proof.mint.mint",
    familyPaths: [],
  },
  fieldPreimageCertificateSpend: {
    purpose: "spend",
    reference: {
      role: "V1 field-preimage certificate",
      token: "V1FieldPreimageCertSpend",
    },
    commands: [],
    validatorTitle:
      "field_preimage_certificate.field_preimage_certificate.spend",
    familyPaths: [],
  },
  fieldPreimageCertificateMint: {
    purpose: "mint",
    reference: {
      role: "V1 field-preimage certificate minting",
      token: "V1FieldPreimageCertMint",
    },
    commands: [],
    validatorTitle:
      "field_preimage_certificate.field_preimage_certificate.mint",
    familyPaths: [],
  },
  fieldGrammarCertificateSpend: {
    purpose: "spend",
    reference: {
      role: "V1 field grammar certificate",
      token: "V1FieldGrammarCertSpend",
    },
    commands: [],
    validatorTitle: "field_grammar_certificate.field_grammar_certificate.spend",
    familyPaths: [],
  },
  fieldGrammarCertificateMint: {
    purpose: "mint",
    reference: {
      role: "V1 field grammar certificate minting",
      token: "V1FieldGrammarCertMint",
    },
    commands: [],
    validatorTitle: "field_grammar_certificate.field_grammar_certificate.mint",
    familyPaths: [],
  },
  scriptDecodeSpend: {
    purpose: "spend",
    reference: { role: "V1 script decode", token: "V1ScriptDecodeSpend" },
    commands: [],
    validatorTitle: "script_decode_v1.script_decode_v1.spend",
    familyPaths: [],
  },
  scriptDecodeMint: {
    purpose: "mint",
    reference: {
      role: "V1 script decode minting",
      token: "V1ScriptDecodeMint",
    },
    commands: [],
    validatorTitle: "script_decode_v1.script_decode_v1.mint",
    familyPaths: [],
  },
  scriptDecodeTermWithdraw: {
    purpose: "withdraw",
    reference: {
      role: "V1 script decode term staker",
      token: "V1ScriptDecodeTermStake",
    },
    commands: [],
    validatorTitle: "script_decode_v1.script_decode_term_v1.withdraw",
    familyPaths: [],
  },
  scriptDecodeValueWithdraw: {
    purpose: "withdraw",
    reference: {
      role: "V1 script decode value staker",
      token: "V1ScriptDecodeValueStake",
    },
    commands: [],
    validatorTitle: "script_decode_v1.script_decode_value_v1.withdraw",
    familyPaths: [],
  },
  scriptDecodeListWithdraw: {
    purpose: "withdraw",
    reference: {
      role: "V1 script decode list staker",
      token: "V1ScriptDecodeListStake",
    },
    commands: [],
    validatorTitle: "script_decode_v1.script_decode_list_v1.withdraw",
    familyPaths: [],
  },
  scriptDecodeFoldWithdraw: {
    purpose: "withdraw",
    reference: {
      role: "V1 script decode fold staker",
      token: "V1ScriptDecodeFoldStake",
    },
    commands: [],
    validatorTitle: "script_decode_v1.script_decode_fold_v1.withdraw",
    familyPaths: [],
  },
  scriptDecodeBytesWithdraw: {
    purpose: "withdraw",
    reference: {
      role: "V1 script decode bytes staker",
      token: "V1ScriptDecodeBytesStake",
    },
    commands: [],
    validatorTitle: "script_decode_v1.script_decode_bytes_v1.withdraw",
    familyPaths: [],
  },
  scriptDecodeDataWithdraw: {
    purpose: "withdraw",
    reference: {
      role: "V1 script decode data staker",
      token: "V1ScriptDecodeDataStake",
    },
    commands: [],
    validatorTitle: "script_decode_v1.script_decode_data_v1.withdraw",
    familyPaths: [],
  },
  phasMembershipWithdraw: {
    purpose: "withdraw",
    reference: {
      role: "membership proof withdrawal",
      token: "MembershipProofWithdraw",
    },
    commands: ["phas-membership"],
    validatorTitle: "phas.membership.withdraw",
    familyPaths: [],
  },
  computationThreadMint: {
    purpose: "mint",
    reference: {
      role: "V1 fraud-proof computation-thread minting",
      token: "V1FpComputationThreadMint",
    },
    commands: [],
    validatorTitle: "computation_thread.mint.mint",
    familyPaths: [],
  },
  chunkedVerifyWithdraw: {
    purpose: "withdraw",
    reference: {
      role: "V1 MPF chunked-verify withdrawal",
      token: "V1MpfChunkedVerifyWithdraw",
    },
    commands: [],
    validatorTitle: "mpf_chunked_verify.verify.withdraw",
    familyPaths: [],
  },
  pexcludesWithdraw: {
    purpose: "withdraw",
    reference: {
      role: "V1 MPF pexcludes withdrawal",
      token: "V1MpfPexcludesWithdraw",
    },
    commands: [],
    validatorTitle: "pexcludes.exclusion.withdraw",
    familyPaths: [],
  },
});
