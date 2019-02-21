Aux�lio ao projeto de georreferenciamento de IPTU e Alvar�s do munic�pio de S�o Paulo

pasta data
	pasta IPTU - cont�m os dados georreferenciados do IPTU

pasta results - cont�m os arquivos salvos dos resultados

Classes_IPTU_georref.R - processamento dos arquivos georreferencidados
	Descarta colunas desnecess�rias, padroniza os nomes das colunas e empilha todas as tabelas
	Corrige problemas de encoding e categoriza por tipos a partir da lei 10.235/86
	Filtra apenas os pontos georreferenciados
	Salva arquivo "IPTU_2016_completo.csv"
	Divide o arquivo completo em menores por tipo e os salva

Classes_IPTU_part6.R - processamento do arquivo de endere�os n�o localizados
	Faz o mesmo procedimento de georref, mas n�o filtra os pontos georreferenciados
	Salva arquivo "IPTU_2016_naoLocalizados.csv"