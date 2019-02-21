Auxílio ao projeto de georreferenciamento de IPTU e Alvarás do município de São Paulo

pasta data
	pasta IPTU - contém os dados georreferenciados do IPTU

pasta results - contém os arquivos salvos dos resultados

Classes_IPTU_georref.R - processamento dos arquivos georreferencidados
	Descarta colunas desnecessárias, padroniza os nomes das colunas e empilha todas as tabelas
	Corrige problemas de encoding e categoriza por tipos a partir da lei 10.235/86
	Filtra apenas os pontos georreferenciados
	Salva arquivo "IPTU_2016_completo.csv"
	Divide o arquivo completo em menores por tipo e os salva

Classes_IPTU_part6.R - processamento do arquivo de endereços não localizados
	Faz o mesmo procedimento de georref, mas não filtra os pontos georreferenciados
	Salva arquivo "IPTU_2016_naoLocalizados.csv"