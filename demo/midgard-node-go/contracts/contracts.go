package contracts

import (
	"encoding/hex"
	"encoding/json"
	"errors"
	"fmt"
	"io"
	"os"

	"github.com/Salvionied/apollo/apollotypes"
	"github.com/Salvionied/apollo/serialization/PlutusData"
)

type ContractInfo struct {
	AlwaysSucceedsSpendScript PlutusData.PlutusV3Script
	AlwaysSucceedsMintScript  PlutusData.PlutusV3Script
}

func GetContracts() (apollotypes.AikenPlutusJSON, error) {
	f, err := os.Open("./blueprints/always-succeeds/plutus.json")
	if err != nil {
		return apollotypes.AikenPlutusJSON{}, err
	}
	defer f.Close()
	aikenPlutusJSON := apollotypes.AikenPlutusJSON{}
	aikenPlutusJSON.GetScript("")
	plutusBytes, err := io.ReadAll(f)
	if err != nil {
		return apollotypes.AikenPlutusJSON{}, err
	}
	err = json.Unmarshal(plutusBytes, &aikenPlutusJSON)
	if err != nil {
		return apollotypes.AikenPlutusJSON{}, err
	}
	return aikenPlutusJSON, err
}

func GetScript(apj apollotypes.AikenPlutusJSON, contractName string) ([]byte, error) {
	for _, validator := range apj.Validators {
		if validator.Title == contractName {
			ds, err := hex.DecodeString(validator.CompiledCode)
			if err != nil {
				return nil, err
			}
			return ds, nil
		}
	}
	return nil, errors.New(fmt.Sprintf("contract %s not found", contractName))
}

func GetContractInfo() (*ContractInfo, error) {
	apj, err := GetContracts()
	if err != nil {
		return nil, err
	}
	spendDs, err := GetScript(apj, "always_succeeds.spend.else")
	if err != nil {
		return nil, err
	}

	mintDs, err := GetScript(apj, "always_succeeds.mint.else")
	if err != nil {
		return nil, err
	}
	return &ContractInfo{
		AlwaysSucceedsSpendScript: PlutusData.PlutusV3Script(spendDs),
		AlwaysSucceedsMintScript:  PlutusData.PlutusV3Script(mintDs),
	}, nil
}
