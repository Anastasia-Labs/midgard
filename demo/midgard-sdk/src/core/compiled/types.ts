import { AddressData } from "@/tx-builder/common.js";
import { Address, Script } from "@lucid-evolution/lucid";

export type CborHex = string;

export type ValidatorScript = {
  spending: string;
  minting: string;
  staking: string;
};

export type MultiValidator = {
  spendValidator: Script;
  spendAddress: Address;
  mintValidator: Script;
  mintAddress: Address;
};
