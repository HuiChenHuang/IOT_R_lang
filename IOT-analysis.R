install.packages("RJDBC")
library(RJDBC)

drv <- JDBC(driverClass = "com.mysql.jdbc.Driver", classPath = "D:/mysql-connector-java-5.1.46.jar")
