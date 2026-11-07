/** The validation-trace dispute runner from Init to conviction at the
 * deepest trace admission takes, the operator's thread at the trace's last
 * CEK builtin step, an expModInteger failure with no inverse at the
 * heaviest shape the protocol version 11 arms prove: twice consecutive
 * Fibonacci numbers as base and modulus (56 64-bit words), whose
 * greatest-common-divisor walk is the longest that modulus allows. The
 * successor's thread is at its last direct scalar builtin step, each
 * resolved through its arm chain
 * (validation-trace-dispute-max-shape-case.ts). The same transaction in an
 * honest block, which stamps it TxIsInvalid, starts no workflow and signs
 * nothing; nor does a transaction whose inverse exists, in an honest block
 * that accepts it. */
import { beforeAll, describe, expect, it } from "vitest";

import { ADMITTED_TX_EX_UNITS } from "./helpers/plutus-execution-block-fixture.js";
import {
  deepestCase,
  honestBlock,
  TIMEOUT,
} from "./support/validation-trace-dispute-max-shape-case.js";
import { flatScript } from "./support/validation-trace-dispute-one-step-cek-builtin-drive.js";
import {
  type Scene,
  setUpScene,
} from "./support/validation-trace-dispute-open-scene.js";
import { makeRunner } from "./support/validation-trace-dispute-runner.js";

/**
 * `(program 1.1.0 (lam ctx [(lam _ [(builtin expModInteger) 2F(5160) -1
 * 2F(5161)]) (countdown 5147 0)]))`, F(n) the n-th Fibonacci number: the
 * countdown runs to the per-transaction memory cap and the base has no
 * inverse modulo the modulus, their common divisor being 2.
 */
const FAILING = flatScript(
  "0101002323337ae904077c8f6d1ebc86b5974747bd07ed95f64f772e9746cf76b5e4ccfe2dadde25bcb7878df62cafd63cc5a7adc4ffe7ecfd9fed1c4f4f2f4d16f467bcfe5f5d4fcf0f5f25346576166fe6d5fddc76b76d7c1565ddd6bf275c25ddbd754c8c96ff14b6e72c6fb70f4f551c2cb7557f45cc041c34cf6e2585fded95056ec63e36774f4637cc6d3e0d7f8f36579ef6b7f6e68e07ccb7bfec37aeef6655adbe26668da5258e4d25efc5dde717e715474c7ecffc17f7365545f6a68537ad4f4ea6b53d077407ccbe07a7ed1dde7c9c0f3e95ac57e7bc94d41cbcccacefad5cdce595dfbe86476e5eb6bf1d0fe6772674ad77255ebd3dce1c0f56c50f2d7eeeefa52d1f1d345f454f64ee0706479f5fa46f4cb4470c74f7340c0dbcf78f667df7efeca6e6275e7f2dfe56bfd7551d0f0ff54c65e79faeaf0e9ffd749c7c57a58c2e9fd53c7c2c76043404cfc6adef355d342dd4b5bfd7fdf417cc7f2cd47d44e7befccd4715458d851efe370f3d3d7724cc7ca494efbd47f64c153e64bd75ed3c4fa7667436b7fe949edfbce5fe074f242dcd5f8e8f1ee6455d7de61d878dde3425e5247c9d8c4f4485fe4c9c7c57adc6774dee4f16d5cdac2dfc9525ef6e745f8566efafb49706efadcf072eef7ce557a5c5cecf96e54e4f3e7fb4b4edff46ff3f5d85376f65a63c8dc52515d6565fed34aff67fdf0fe657f6df7d0f9c65fc575f079a2a4002906245fa7bc562d364e4d5e94d6bc0db4dc663ddc2706cc4f3f9c1ed5deb594444cd6056796375e0e64356c66a575ae0576765fef4d0d17e776e45ddf9d05f40de77c653fbd660d0c0f066eec3d555596fd3c674f1dfd757486ce4d566e3e2d6517d4768dc496f456479c350db67e6c9e0de6ee35bfbeb51417873d2e2604df165f3f8e6e847d64d6d51fa4ac6655746eaf06241484ef645edd3f5c7ef7f7ef4695aca6df7ed67e3ed67dbe3f7d24f47ee67de6378d055cbf846e970e1d7cfc540f2ccd8e14cf36b65e8cb71ef75584dc877625a4e497473544e64d5746073c352dcf3fd5f6ad9ea79e6424c64c46aed656cf8cbf8ff6f5b79c8f6656fcd6a7acb50d3f24b6efe67ed75eed451fdfbe94eefd3f0e7f7c45acb5d42666cfa525ddfcdc3c9d9f0cb56ea64f67eda64726fe0f95c7b4edbc0407ed06f4573cddced6377cf4f77db6461cfe1d25ed2e1ffdf4348e7ccfe44536cffc1c761fd7dc35660e7c95379755457cfcf56697d49d2c661cbec41e3f142d17f7d757566e8cedfcf5dd859d17dc0e949575eced44f4548e0ea78e8e5fc79c54368e2cffce5714d72f35cedec42c679ccd863fa6d6bceef6764f8e0f34ef95e75cf4ef5e95d4075e17fff516c47c265634fe95357f8c857cc516d42fe4663557b4447c47ee7766342c26de86558754b47fac8f2f271504956ec7a7b7bcbf3fdf3cc656edd4671c6b89999191801119801001000918011198010010009112999ab9a3370e0049000080089980199b810024800800520b650480001",
);

/**
 * `(program 1.1.0 (lam ctx [(lam _ ()) [(builtin expModInteger) F(5160) -1
 * F(5161)]]))`: consecutive Fibonacci numbers are coprime, so the inverse
 * exists and the script returns unit.
 */
const PASSING = flatScript(
  "010100232498ccdeba4181ef91eda35711573369e977a17d33bf49ef6553e9596f57bd999fc5b53bc53717f171bfc515fb4719b575b91ffdfd9fb3fda389e9e569235f8df79fcbeba9f9e16b65270d2fc34d7ddbbf3b0fd7ed2f03ad3bbb57e56b85bb37afa911935f6317dde50df7e169eba30517eb2fe939810387996d45b1bf3d33a12dd94747cf6969c7f98da7c1af7167cb735f577fdd51c17997f77d87f55d6dcbb53745cd5135a5b149a53df93b3d637d63a9e98fd97f037f674b293f5551a7f5a96955d727216f81f91741f5fda3bbcf938167d3350bfd77139b839799959df5ab1b1db3bb7751c9ed4bd7d7e3217d4f65cf152fe5abd7a7b9c3016bd9a1e5afdd5df5a5a363a70be929ed1d4161c9f36bf58d691789610f1fe78181379f71ed4fbffd7d155dc5ebcfe53fcb577beba3a161ff290dbdf3f5d5e1d37faf930f0bf5b18553fba78f050f41078119f9d53de72ba7053b9737fb7f3f83f98f659b0f299df7df192963a931b1a35fc7e1e7272fe5990f15939d7729ff498327cd17afbda709756d4f07d77fd393db779d3fc169e585b9abf1d1635dc9ab2fbd43b1f13b47053da58f93b1096991bfc9930f8b7539cfe9bd4963dbb9b5853f13a5bd6dcf0b71addd757713e1ddf539e1e5dd6f1d2b75b9b95973dda9c9e74f77979d3fe9dfe76b31a76d6db5c7113925233bcbcb7da715ffcffb617d4bffdbefa1738d3f8b6be1f2449000a41c50bf5778b45a7c9c92b539bd701b71b8dc73b05e1d989e7f303db3b573389091b412d73c7eb414d872d0d55af35412fcfcbfde921237d6fdd8bbb7321bf01bd6f8da777adc1a101e1cddd072b2bb3df278de9e33f2f2f91d949abcdc7452d237b8f5139135f0bc9738721b7cfcd9341bd5dc7b777572303f1e72545c11be3cbe7f14dd10f2d1bdb23f5150d4baf8d55614503911ded8bdba7eb0f5fff7d69d33595db6fdbcf47dbcfb7c76f259f0fdd4f3dc771a1ab17f10dd3e1c3af1f8b81e59931c31967d7cb5197635f6bb11b116f45351d136967299d492b69c16787a5b9677bbfd533d5734d05994989555bcbd9f197717f5fb7f3116dcb5f1bd57597a12765975dfd4fdbeb5da9a3fb77d39ddfa7e1cf6f8935173b05cd5975a5bbbf9b8793b361972dd549ed7d3549e5df417339f79d3701817d215f8be79b395bc76f1f9f6f37c9c39f43a5bda5c37f3f8791cf197d09a7d9ff038f43fb7b07adc14f1327736ba9af1f1f2d53fb93258dc317d98347e305237f7b6bebcdd19d3f9f3bb133a3fb015313afbd1d291f8b9141d5f1d14bf9730b87d1c59f794b639b65e7b95bd9058df319b14775dbd71d5fcfc9f141e71d73bd6b9f9d6b533b816bc37f7f23d90f054bc71f53a7af71912f1923db057d0d472b77890f897d4f6dc705855b514b316b978ff59165656321932d5975f7f797e7fb6719cb5d3b8de38c701",
);

describe("validation-trace dispute runner at the deepest trace admission takes", () => {
  deepestCase({
    failingScript: FAILING,
    group: 14,
    successor: { group: 3 },
  });

  honestBlock({ failingScript: FAILING });

  describe("a transaction whose inverse exists, in an honest block", () => {
    let scene: Scene;

    beforeAll(async () => {
      scene = await setUpScene(undefined, {
        passingSpend: { script: PASSING, exUnits: ADMITTED_TX_EX_UNITS },
      });
    }, TIMEOUT);

    it(
      "starts no workflow and signs nothing",
      async () => {
        const unspent = () =>
          Object.values(scene.h.emulator.ledger)
            .filter(({ spent }) => !spent)
            .map(({ utxo }) => `${utxo.txHash}#${utxo.outputIndex.toString()}`);
        const signed = new Set(scene.l1.signedCbors.keys());
        const before = unspent();
        const result = await makeRunner(scene).runOnce(scene.passing);
        expect(result).toMatchObject({ kind: "no_fault_detected" });
        expect(
          [...scene.l1.signedCbors.keys()].filter((hash) => !signed.has(hash)),
        ).toStrictEqual([]);
        expect(unspent()).toStrictEqual(before);
      },
      TIMEOUT,
    );
  });
});
