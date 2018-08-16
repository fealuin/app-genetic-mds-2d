-- MySQL Workbench Forward Engineering

SET @OLD_UNIQUE_CHECKS=@@UNIQUE_CHECKS, UNIQUE_CHECKS=0;
SET @OLD_FOREIGN_KEY_CHECKS=@@FOREIGN_KEY_CHECKS, FOREIGN_KEY_CHECKS=0;
SET @OLD_SQL_MODE=@@SQL_MODE, SQL_MODE='TRADITIONAL,ALLOW_INVALID_DATES';

-- -----------------------------------------------------
-- Schema memoria
-- -----------------------------------------------------

-- -----------------------------------------------------
-- Schema memoria
-- -----------------------------------------------------
CREATE SCHEMA IF NOT EXISTS `memoria` DEFAULT CHARACTER SET utf8 ;
USE `memoria` ;

-- -----------------------------------------------------
-- Table `memoria`.`individuals`
-- -----------------------------------------------------
CREATE TABLE IF NOT EXISTS `memoria`.`individuals` (
  `individual_id` INT NOT NULL,
  `point_id` INT NOT NULL,
  `x` DOUBLE NULL,
  `y` DOUBLE NULL,
  `parameters_id` INT NOT NULL,
  `run` INT NOT NULL,
  PRIMARY KEY (`individual_id`, `point_id`, `parameters_id`, `run`))
ENGINE = InnoDB
DEFAULT CHARACTER SET = utf8;


-- -----------------------------------------------------
-- Table `memoria`.`parameters`
-- -----------------------------------------------------
CREATE TABLE IF NOT EXISTS `memoria`.`parameters` (
  `id` INT UNSIGNED NOT NULL,
  `dataset` VARCHAR(45) NULL,
  `initialization` VARCHAR(45) NULL,
  `runs` INT UNSIGNED NULL,
  `radio` DOUBLE NULL,
  `population_size` INT UNSIGNED NULL,
  `generations` INT UNSIGNED NULL,
  PRIMARY KEY (`id`,`runs`))
ENGINE = InnoDB;


-- -----------------------------------------------------
-- Table `memoria`.`results`
-- -----------------------------------------------------
CREATE TABLE IF NOT EXISTS `memoria`.`results` (
  `parameters_id` INT UNSIGNED NOT NULL,
  `individual_id` INT UNSIGNED NOT NULL,
  `generation` INT UNSIGNED NOT NULL,
  `x` DOUBLE NULL,
  `y` DOUBLE NULL,
  `rank` INT NULL,
  `crowding` VARCHAR(45) NULL,
  `run` INT NOT NULL,
  PRIMARY KEY (`parameters_id`, `individual_id`, `generation`, `run`))
ENGINE = InnoDB;


SET SQL_MODE=@OLD_SQL_MODE;
SET FOREIGN_KEY_CHECKS=@OLD_FOREIGN_KEY_CHECKS;
SET UNIQUE_CHECKS=@OLD_UNIQUE_CHECKS;
