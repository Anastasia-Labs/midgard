import {
  type MidgardCekBuiltinName,
  midgardCekBuiltinTagOfName,
} from "../../src/cek-builtin.cardano-tags.js";
import { Builtin } from "../../src/uplc-term.js";

/**
 * Midgard's builtin terms by name, for tests that build terms by hand. The
 * names are the textual UPLC names aiken reads; the term holds Cardano's tag
 * for that builtin, as every Midgard term does.
 */
export type BuiltinName = MidgardCekBuiltinName;

/** A fresh builtin term for the builtin `name`. */
export const builtin = (name: BuiltinName): Builtin =>
  new Builtin(midgardCekBuiltinTagOfName(name));
