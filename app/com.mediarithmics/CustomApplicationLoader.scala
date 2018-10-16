package com.mediarithmics

import cats.effect.internals.IOContextShift
import com.google.inject.{AbstractModule, Provides, Singleton}
import com.mediarithmics.DB.TransactionableIO.TransactionableIO
import com.zaxxer.hikari.HikariDataSource
import org.hibernate.SessionFactory
import org.hibernate.boot.MetadataSources
import org.hibernate.boot.registry.StandardServiceRegistryBuilder
import org.hibernate.cfg.AvailableSettings
import play.api.ApplicationLoader
import play.api.inject.guice.{GuiceApplicationBuilder, GuiceApplicationLoader}
import play.api.db.DBApi

class CustomApplicationLoader extends GuiceApplicationLoader() {
  override def builder(context: ApplicationLoader.Context): GuiceApplicationBuilder = {
    super.builder(context).bindings(new CustomModule)
  }
}

class CustomModule extends AbstractModule {
  override def configure(): Unit = ()

  @Singleton
  @Provides
  def provideDBMonad(DBApi: DBApi): SessionFactory = {

    val dataSource = DBApi.databases().map(_.dataSource).collect {
      case ds: HikariDataSource => ds
    }.head

    val registryBuilder = new StandardServiceRegistryBuilder()

    val registry = registryBuilder.applySetting(AvailableSettings.DATASOURCE, dataSource)
      .configure // configures settings from hibernate.cfg.xml
      .build

      new MetadataSources(registry)
        .buildMetadata
        .buildSessionFactory
  }


  @Singleton
  @Provides
  def provideDBMonad(sessionFactory: SessionFactory): DB[TransactionableIO] =
    DB.ioInstance(() => sessionFactory.createEntityManager())(IOContextShift.global)

  @Singleton
  @Provides
  def provideUserService(DB: DB[TransactionableIO]): UserService[TransactionableIO] =
    new UserService[TransactionableIO]()(DB)

  @Singleton
  @Provides
  def provideGroupService(DB: DB[TransactionableIO]): GroupService[TransactionableIO] =
    new GroupService[TransactionableIO]()(DB)

}