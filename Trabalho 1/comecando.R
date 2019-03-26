library(haven)
library(dplyr)
library(magrittr)
library(ggplot2)
library(gridExtra)

# Importacao base de dados
base <- 
  read_sav("UFF/Estatistica Aplicada/Trabalho 1/BancoTeseCris_09_07_2013.sav") %>% 
  mutate(Sexo = factor(Sexo, labels = c("Homem", "Mulher")))

# Visualizacao sexo por idade
base %>%  
  select(Sexo, Idade) %>% 
  na.omit() %>% 
  ggplot(aes(x=Sexo, y=Idade)) + 
  geom_boxplot() +
  labs(x = "Sexo", title = "Boxplot Idade por Sexo") + 
  theme_bw()

# 37 pacientes tiveram o sangue coletado

base_mod <- 
  base %>%
  select(Sexo, Idade,
         # Parametros antropometricos
         IMC1, IMC2, 
         AMBc1, AMBc2, 
         obeso, dif_obes,
         Mas_Magra1, Mas_Magra2, 
         # Parametros bioquimicos
         PCR01, PCR02,
         ICAM1, ICAM2,
         VCAM1, VCAM2,
         IL601, IL602,
         TNFa01, TNFa02,
         Albumina01, albumina02,
         Creatinina01, Creatinina02,
         Hb01, Hb02, #hemoglobina
         KTV01, KTV02,
         # PCR01, PCR02, #C-reactive protein
         # Capacidade fisica
         SL101, SL102, #SLS
         SL601, SL602,
         TorqueextNmE1, TorqueextNmE2,
         TorqueextNmD1, TorqueextNmD2,
         TorqueFLX.NmE01, TorqueFLX.NmE02,
         TorqueFLX.NmD01, TorqueFLX.NmD02,
         # PEW
         PEW_ANTES, PEW_APOS # presenca = 2
         ) %>% 
  mutate(
    dif_imc = IMC2-IMC1,
    dif_amb = AMBc2-AMBc1,
    dif_mas_magra = Mas_Magra2-Mas_Magra1,
    dif_pcr = PCR02-PCR01,
    dif_icam = ICAM2-ICAM1,
    dif_vcam = VCAM2-VCAM1,
    dif_il6 = IL602-IL601,
    # ffm_div_il6 = 
    dif_tnfa = TNFa02-TNFa01,
    dif_albumina = albumina02-Albumina01,
    dif_creatinina = Creatinina02-Creatinina01,
    dif_hemoglobina = Hb02-Hb01,
    dif_kt = KTV02-KTV01,
    dif_sl10 = SL102-SL101,
    dif_sl60 = SL602-SL601,
    dif_torq_ext_e = TorqueextNmE2-TorqueextNmE1,
    dif_torq_ext_d = TorqueextNmD2-TorqueextNmD1,
    dif_torq_flx_e = TorqueFLX.NmE02-TorqueFLX.NmE01,
    dif_torq_flx_d = TorqueFLX.NmD02-TorqueFLX.NmD01
  )


# PEW
base_pew <- 
  base %>% 
  select(
    Sexo, Idade, 
    PEW_ANTES, PEW_APOS
  ) %>% na.omit()

# Visualizacao PEW ANTES
graf_pew_antes <- 
  base_pew$PEW_ANTES %>% table() %>% as_tibble() %>% 
  ggplot() +
  geom_bar(aes(x=., y=n), stat = "identity") +
  theme_bw() +
  labs(x = "Presença", y = "Número de pacientes", title = "PEW ANTES do RETP nos pacientes") +
  scale_y_continuous(limits = c(0, 30))

# Visualizacao PEW APOS
graf_pew_apos <- 
  base_pew$PEW_APOS %>% table() %>% as_tibble() %>% 
  ggplot() +
  geom_bar(aes(x=., y=n), stat = "identity") +
  theme_bw() +
  labs(x = "Presença", y = "Número de pacientes", title = "PEW APOS do RETP nos pacientes") + 
  scale_y_continuous(limits = c(0, 30))

grid.arrange(graf_pew_antes, graf_pew_apos, ncol = 2)

# Hipotese: O estudo teve efeito sobre PEW? 

  





# Marcadores de Inflamacao
base_inflamacao <- 
  base %>% 
  select(
    Sexo, Idade,
    TNFa01, TNFa02,
    ICAM1, ICAM2,
    PCR01, PCR02,
    IL601, IL602,
    VCAM1, VCAM2
  ) %>% na.omit()

base_pew 
  



base %>% 
  select(Sexo, Idade,
         # Parametros antropometricos
         IMC1, IMC2,
         AMBc1, AMBc2,
         obeso,
         Mas_Magra1, Mas_Magra2) %>% 
  na.omit()














# crp, FFM/IL6, FFM/TNFa, STS

# o que e ICAM1 e ICAM2, VCAM1, VCAM2, 

base$obeso

# Inflamatorios, Energetica
# impacto 