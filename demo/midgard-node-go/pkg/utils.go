package pkg

import (
	"bytes"
	"os"
	"path/filepath"
	"strings"

	"github.com/spf13/cobra"
	"github.com/spf13/viper"
)

var configValue string

func AddConfigFlag(cmd *cobra.Command) {
	cmd.PersistentFlags().StringVarP(&configValue, "config-file", "c", "./config/config.toml", "config file?")
}

func LoadConfig() {
	configFiles := strings.Split(configValue, ";")
	err := ReadConfig(viper.GetViper(), configFiles)
	if err != nil {
		panic("LoadConfig: " + err.Error())
	}
}

func ReadConfig(vi *viper.Viper, files []string) error {
	for i, file := range files {
		if _, err := os.Stat(file); os.IsNotExist(err) {
			return err
		}

		value, err := os.ReadFile(file)
		if err != nil {
			return err
		}

		ext := filepath.Ext(file)[1:]
		vi.SetConfigType(ext)
		reader := bytes.NewBuffer(value)
		if i == 0 {
			err = vi.ReadConfig(reader)
		} else {
			err = vi.MergeConfig(reader)
		}
		if err != nil {
			return err
		}
	}

	return nil
}
