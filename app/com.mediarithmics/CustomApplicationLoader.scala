package com.mediarithmics

import cats.effect.{Async, IO}
import cats.effect.internals.IOContextShift
import com.google.inject.{AbstractModule, Provides, Singleton}
import com.mediarithmics.DB.Transactioner.TransactionState
import com.mediarithmics.iodb._
import com.zaxxer.hikari.HikariDataSource
import javax.persistence.EntityManager
import org.hibernate.SessionFactory
import org.hibernate.boot.MetadataSources
import org.hibernate.boot.registry.StandardServiceRegistryBuilder
import org.hibernate.cfg.AvailableSettings
import play.api.ApplicationLoader
import play.api.db.DBApi
import play.api.inject.guice.{GuiceApplicationBuilder, GuiceApplicationLoader}

class CustomApplicationLoader extends GuiceApplicationLoader() {
  override def builder(context: ApplicationLoader.Context): GuiceApplicationBuilder = {
    super.builder(context).bindings(new CustomModule)
  }
}

class CustomModule extends AbstractModule {
  override def configure(): Unit = ()

  @Singleton
  @Provides
  def provideSessionFactory(DBApi: DBApi): SessionFactory = {

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
  def provideDB(sessionFactory: SessionFactory): DB[TransactionableIO, EntityManager] =
    ioInstance(() => sessionFactory.createEntityManager())(IOContextShift.global)

  @Singleton
  @Provides
  def provideUserService(DB: DB[TransactionableIO, EntityManager],
                         Async: Async[TransactionableIO]
                        ): UserService[TransactionableIO, EntityManager] = {

    new UserService[TransactionableIO, EntityManager]()(DB, Async)
  }

  @Singleton
  @Provides
  def provideGroupService(DB: DB[TransactionableIO, EntityManager],
                          Async: Async[TransactionableIO]): GroupService[TransactionableIO, EntityManager] = {
    new GroupService[TransactionableIO, EntityManager]()(DB, Async)
  }

  @Singleton
  @Provides
  def provideEffect: Async[TransactionableIO] = {
    Async.catsKleisliAsync[IO, TransactionState](IO.ioEffect)
  }

}